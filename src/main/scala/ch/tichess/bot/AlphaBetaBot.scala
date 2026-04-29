package ch.tichess.bot

import ch.tichess.controller.AppState
import ch.tichess.model.{Board, Color, Game, Move, Piece, PieceType, Pos}

import scala.concurrent.Future
import scala.collection.mutable
import ch.tichess.model.Fen

/**
 * Simple alpha-beta bot with a lightweight material-based heuristic.
 *
 * Note: this is intended as an MVP and therefore keeps the evaluation cheap.
 */
class AlphaBetaBot(thinkTimeMs: Long = 5000L, openingDb: Option[OpeningDatabase] = None) extends ChessBot:
  override val name: String = s"AlphaBetaBot(time=${thinkTimeMs}ms)"

  private class TimeLimitExceededException extends RuntimeException

  private enum Bound:
    case Exact, Lower, Upper

  private final case class TranspositionEntry(depth: Int, score: Int, bound: Bound, bestMove: Option[Move])
  private final class SearchProfiler:
    val enabled: Boolean = sys.env.get("TICHESS_PROFILE").exists(_.equalsIgnoreCase("true"))
    var legalNanos: Long = 0L
    var legalCalls: Long = 0L
    var applyMoveNanos: Long = 0L
    var applyMoveCalls: Long = 0L
    var evaluateNanos: Long = 0L
    var evaluateCalls: Long = 0L
    var inCheckNanos: Long = 0L
    var inCheckCalls: Long = 0L
    var orderNanos: Long = 0L
    var orderCalls: Long = 0L
    var hashNanos: Long = 0L
    var hashCalls: Long = 0L
    var ttNanos: Long = 0L
    var ttCalls: Long = 0L

    def reset(): Unit =
      legalNanos = 0L
      legalCalls = 0L
      applyMoveNanos = 0L
      applyMoveCalls = 0L
      evaluateNanos = 0L
      evaluateCalls = 0L
      inCheckNanos = 0L
      inCheckCalls = 0L
      orderNanos = 0L
      orderCalls = 0L
      hashNanos = 0L
      hashCalls = 0L
      ttNanos = 0L
      ttCalls = 0L

    def printSummary(totalNanos: Long, nodes: Long, ttSize: Int): Unit =
      if enabled then
        println(s"  profile | total ${formatNanos(totalNanos)} | nodes $nodes | tt $ttSize")
        printLine("legal", legalNanos, legalCalls, totalNanos)
        printLine("apply", applyMoveNanos, applyMoveCalls, totalNanos)
        printLine("eval", evaluateNanos, evaluateCalls, totalNanos)
        printLine("check", inCheckNanos, inCheckCalls, totalNanos)
        printLine("order", orderNanos, orderCalls, totalNanos)
        printLine("hash", hashNanos, hashCalls, totalNanos)
        printLine("tt", ttNanos, ttCalls, totalNanos)

    private def printLine(label: String, nanos: Long, calls: Long, totalNanos: Long): Unit =
      val pct = if totalNanos > 0 then nanos.toDouble * 100.0 / totalNanos else 0.0
      val avg = if calls > 0 then nanos / calls else 0L
      val total = formatNanos(nanos)
      val average = formatNanos(avg)
      println(f"    $label%-6s $total%8s | $pct%5.1f%% | calls $calls | avg $average")

    private def formatNanos(nanos: Long): String =
      if nanos >= 1000000L then f"${nanos / 1000000.0}%.2fms"
      else if nanos >= 1000L then f"${nanos / 1000.0}%.2fus"
      else s"${nanos}ns"

  private final class SearchContext:
    val transpositionTable: mutable.HashMap[Long, TranspositionEntry] = mutable.HashMap.empty
    val killerMoves: mutable.HashMap[Int, List[Move]] = mutable.HashMap.empty
    val historyScores: mutable.HashMap[Move, Int] = mutable.HashMap.empty
    val nodes = java.util.concurrent.atomic.AtomicLong(0)
    val profiler = SearchProfiler()

  private val context = SearchContext()
  private val maxTranspositionEntries = 1000000
  private val syzygyTablebase = SyzygyTablebase.fromEnv()

  override def chooseMove(state: AppState, remainingTimeMs: Option[Long] = None): Future[Either[String, Move]] =
    val game = state.game
    val legal = game.legalMoves
    if legal.isEmpty then Future.successful(Left("No legal moves available."))
    else
      // Dynamic time management: 10m -> 10s, 5m -> 5s, 1m -> 1s (time / 60)
      // Cap at 10 seconds to avoid excessive thinking in unlimited/long games.
      val budget = remainingTimeMs match
        case Some(ms) =>
          if ms > 5 * 60 * 1000 then 10000L      // Über 5 Min: 10s
          else if ms > 1 * 60 * 1000 then 5000L // 1 bis 5 Min: 5s
          else 1000L                            // Unter 1 Min: 1s
        case None => thinkTimeMs

      val normalizedFen = Fen.encodeNormalized(game)
      
      val dbFuture = openingDb match
        case Some(db) => db.getMoves(normalizedFen)
        case None => Future.successful(List.empty)

      implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

      dbFuture.flatMap { dbMoves =>
        val validDbMoves = dbMoves.filter(m => legal.contains(m.move))
        if validDbMoves.nonEmpty then
          // Pick the best known move from the database
          val bestDbMove = validDbMoves.maxBy(_.score).move
          Future.successful(Right(bestDbMove))
        else syzygyTablebase.flatMap(_.bestMove(game)).filter(legal.contains) match
          case Some(tablebaseMove) =>
            Future.successful(Right(tablebaseMove))
          case None =>
            searchMoveAsync(game, legal, budget)
      }

  private def searchMoveAsync(game: Game, legal: List[Move], budget: Long): Future[Either[String, Move]] =
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    // Execute CPU-heavy search in a global thread pool
    Future {
      val deadline = System.currentTimeMillis() + budget
      val searchStartNanos = System.nanoTime()
      var bestMoveSoFar = legal.head
      var bestScoreSoFar = -mateScore
      var currentDepth = 1
      context.nodes.set(0) // Reset node counter for this specific move display
      context.profiler.reset()

      try
        // Iterative Deepening
        while System.currentTimeMillis() < deadline do
          val startTime = System.nanoTime()
          val nodesBeforeDepth = context.nodes.get()
          val (mv, score) = searchBestMove(game, legal, currentDepth, deadline, context)
          val durationNanos = System.nanoTime() - startTime
          val durationMs = durationNanos / 1000000L
          val totalNodes = context.nodes.get()
          val depthNodes = totalNodes - nodesBeforeDepth
          val nps = if durationNanos > 0 then (depthNodes * 1000000000L) / durationNanos else 0

          bestMoveSoFar = mv
          bestScoreSoFar = score

          // Info-Logging für dich
          val scoreDesc = if Math.abs(score) > mateScore - 500 then s"MATE" else s"${score / 100.0}"
          println(s"  depth $currentDepth | score $scoreDesc | move $mv | nodes $depthNodes/$totalNodes | nps $nps | tt ${context.transpositionTable.size} | ${durationMs}ms")

          // If we found a mate, no need to search deeper
          if score >= mateScore - 100 then throw new TimeLimitExceededException()
          currentDepth += 1
      catch
        case _: TimeLimitExceededException => // Search aborted, keep best move from last completed depth

      context.profiler.printSummary(System.nanoTime() - searchStartNanos, context.nodes.get(), context.transpositionTable.size)
      Right(bestMoveSoFar)
    }

  private def searchBestMove(game: Game, legal: List[Move], depth: Int, deadline: Long, context: SearchContext): (Move, Int) =
    var bestMove = legal.head
    var bestScore = -mateScore * 2

    var alpha = -mateScore * 2
    val beta = mateScore * 2
    val alphaOrig = alpha

    val positionKey = transpositionKey(game, context)
    val ttBestMove = transpositionLookup(context, positionKey).flatMap(_.bestMove)
    val ordered = orderedMoves(game, legal, ttBestMove, ply = 0, context)
    var i = 0
    while i < ordered.size && alpha < beta do
      if System.currentTimeMillis() > deadline then throw new TimeLimitExceededException()

      val mv = ordered(i)
      val next = applyMoveProfiled(game, mv, context)
      val score = -negamax(next, depth - 1, -beta, -alpha, deadline, ply = 1, context)

      if score > bestScore then
        bestScore = score
        bestMove = mv

      alpha = Math.max(alpha, bestScore)
      i += 1

    storeTransposition(context, positionKey, depth, bestScore, boundFor(bestScore, alphaOrig, beta), Some(bestMove), ply = 0)
    (bestMove, bestScore)

  private def negamax(game: Game, depth: Int, alpha0: Int, beta0: Int, deadline: Long, ply: Int, context: SearchContext): Int =
    context.nodes.incrementAndGet()
    if System.currentTimeMillis() > deadline then throw new TimeLimitExceededException()

    // Contempt for draws: penalize them so the bot tries to win
    val contemptValue = -50
    if game.halfMoveClock >= 100 then return contemptValue

    if depth <= 0 then
      if isInCheckProfiled(game, context) then
        val legal = legalMovesProfiled(game, context)
        if legal.isEmpty then -mateScore + ply
        else searchCheckEvasions(game, legal, alpha0, beta0, deadline, ply, context)
      else quiescence(game, alpha0, beta0, deadline, ply, context)
    else
      val positionKey = transpositionKey(game, context)
      transpositionLookup(context, positionKey) match
        case Some(entry) if entry.depth >= depth =>
          val ttScore = scoreFromTransposition(entry.score, ply)
          entry.bound match
            case Bound.Exact => return ttScore
            case Bound.Lower if ttScore >= beta0 => return ttScore
            case Bound.Upper if ttScore <= alpha0 => return ttScore
            case _ =>
        case _ =>

      val legal = legalMovesProfiled(game, context)
      if legal.isEmpty then
        if isInCheckProfiled(game, context) then -mateScore + ply
        else contemptValue // Stalemate / Draw penalty
      else
        val alphaOrig = alpha0
        var alpha = alpha0
        var best = -mateScore * 2
        var bestMove: Option[Move] = None

        val ttBestMove = transpositionLookup(context, positionKey).flatMap(_.bestMove)
        val ordered = orderedMoves(game, legal, ttBestMove, ply, context)

        var i = 0
        while i < ordered.size && alpha < beta0 do
          val mv = ordered(i)
          val next = applyMoveProfiled(game, mv, context)
          val score = -negamax(next, depth - 1, -beta0, -alpha, deadline, ply + 1, context)
          if score > best then
            best = score
            bestMove = Some(mv)
          alpha = Math.max(alpha, best)
          if alpha >= beta0 then recordCutoff(game, mv, depth, ply, context)
          i += 1

        storeTransposition(context, positionKey, depth, best, boundFor(best, alphaOrig, beta0), bestMove, ply)
        best

  private def searchCheckEvasions(game: Game, legal: List[Move], alpha0: Int, beta: Int, deadline: Long, ply: Int, context: SearchContext): Int =
    var alpha = alpha0
    var best = -mateScore * 2
    val ordered = orderedMoves(game, legal, ttBestMove = None, ply, context)
    var i = 0
    while i < ordered.size && alpha < beta do
      if System.currentTimeMillis() > deadline then throw new TimeLimitExceededException()
      val next = applyMoveProfiled(game, ordered(i), context)
      val score = -quiescence(next, -beta, -alpha, deadline, ply + 1, context)
      best = Math.max(best, score)
      alpha = Math.max(alpha, best)
      i += 1
    best

  private def quiescence(game: Game, alpha0: Int, beta: Int, deadline: Long, ply: Int, context: SearchContext): Int =
    context.nodes.incrementAndGet()
    if System.currentTimeMillis() > deadline then throw new TimeLimitExceededException()

    var alpha = alpha0
    val standPat = staticEvaluateProfiled(game, context)
    if standPat >= beta then return beta
    if standPat > alpha then alpha = standPat

    val captures = legalMovesProfiled(game, context).filter(captureUrgency(game, _) > 0)
    val ordered = orderedMoves(game, captures, ttBestMove = None, ply, context)
    var i = 0
    while i < ordered.size && alpha < beta do
      val mv = ordered(i)
      val next = applyMoveProfiled(game, mv, context)
      val score = -quiescence(next, -beta, -alpha, deadline, ply + 1, context)
      if score > alpha then alpha = score
      i += 1

    alpha

  private val mateScore: Int = 100000
  private val mateScoreThreshold: Int = mateScore - 1000

  // ─── PeSTO Evaluation ────────────────────────────────────────────────────

  /**
   * Compute the game phase (0 = pure endgame, PestoTables.totalPhase = full middlegame)
   * based on the non-pawn, non-king material still on the board.
   */
  private def gamePhase(game: Game): Int =
    val phase = game.board.allPieces.values.foldLeft(0) { (acc, piece) =>
      acc + PestoTables.phaseWeight.getOrElse(piece.kind, 0)
    }
    Math.min(phase, PestoTables.totalPhase)

  /**
   * Score all pieces for one color using PeSTO material + PST values,
   * interpolated between middlegame and endgame.
   */
  private def colorScore(game: Game, isWhite: Boolean, phase: Int): Int =
    game.board.allPieces.collect {
      case (pos, piece) if (piece.color == Color.White) == isWhite =>
        PestoTables.pieceScore(piece.kind, pos, isWhite, phase)
    }.sum

  private def staticEvaluate(game: Game): Int =
    val phase = gamePhase(game)
    val whiteScore = colorScore(game, isWhite = true,  phase)
    val blackScore = colorScore(game, isWhite = false, phase)
    val diff =
      whiteScore - blackScore +
        pawnStructureScore(game.board, Color.White) - pawnStructureScore(game.board, Color.Black) +
        kingSafetyScore(game, Color.White) - kingSafetyScore(game, Color.Black) +
        pieceActivityScore(game, Color.White, phase) - pieceActivityScore(game, Color.Black, phase) +
        queenInvasionScore(game, Color.White, phase) - queenInvasionScore(game, Color.Black, phase) +
        mobilityScore(game.board, Color.White, phase) - mobilityScore(game.board, Color.Black, phase) +
        hangingPiecesScore(game.board, Color.White) - hangingPiecesScore(game.board, Color.Black)
    game.sideToMove match
      case Color.White => diff
      case Color.Black => -diff

  private def pawnStructureScore(board: Board, color: Color): Int =
    val ownPawns = pawnsOf(board, color)
    val enemyPawns = pawnsOf(board, color.other)
    val pawnsByFile = ownPawns.groupBy(_.file)

    ownPawns.map { pawn =>
      val doubledPenalty = if pawnsByFile.getOrElse(pawn.file, Nil).size > 1 then -14 else 0
      val isolatedPenalty = if adjacentFiles(pawn.file).forall(file => !pawnsByFile.contains(file)) then -18 else 0
      val backwardPenalty = if isBackwardPawn(pawn, color, ownPawns, enemyPawns) then -10 else 0
      val passedBonus =
        if isPassedPawn(pawn, color, enemyPawns) then
          val advancement = if color == Color.White then pawn.rank else 7 - pawn.rank
          12 + advancement * advancement * 3
        else 0

      doubledPenalty + isolatedPenalty + backwardPenalty + passedBonus
    }.sum

  private def kingSafetyScore(game: Game, color: Color): Int =
    findKing(game.board, color) match
      case None => 0
      case Some(king) =>
        val homeRank = if color == Color.White then 0 else 7
        val castledBonus =
          if king.rank == homeRank && (king.file == 6 || king.file == 2) then 35
          else if king.rank == homeRank && king.file == 4 && hasCastlingRight(game, color) then 10
          else if gamePhase(game) > PestoTables.totalPhase / 2 then -20
          else 0

        val shieldScore = pawnShieldScore(game.board, color, king)
        val openFilePenalty = kingFileExposurePenalty(game.board, color, king)
        castledBonus + shieldScore - openFilePenalty

  private def pawnShieldScore(board: Board, color: Color, king: Pos): Int =
    val dir = pawnDirection(color)
    adjacentFilesInclusive(king.file).map { file =>
      val front = Pos(file, king.rank + dir)
      val farFront = Pos(file, king.rank + 2 * dir)
      if front.inBounds && board.pieceAt(front).contains(Piece(color, PieceType.Pawn)) then 12
      else if farFront.inBounds && board.pieceAt(farFront).contains(Piece(color, PieceType.Pawn)) then 5
      else if front.inBounds then -8
      else 0
    }.sum

  private def kingFileExposurePenalty(board: Board, color: Color, king: Pos): Int =
    adjacentFilesInclusive(king.file).map { file =>
      val ownPawnOnFile = board.allPieces.exists {
        case (pos, Piece(c, PieceType.Pawn)) => c == color && pos.file == file
        case _ => false
      }
      val enemyHeavyOnFile = board.allPieces.exists {
        case (pos, Piece(c, kind)) =>
          c == color.other && pos.file == file && (kind == PieceType.Rook || kind == PieceType.Queen)
      }

      val openPenalty = if ownPawnOnFile then 0 else 14
      val heavyPenalty = if !ownPawnOnFile && enemyHeavyOnFile then 18 else 0
      openPenalty + heavyPenalty
    }.sum

  private def pawnsOf(board: Board, color: Color): List[Pos] =
    board.allPieces.collect {
      case (pos, Piece(c, PieceType.Pawn)) if c == color => pos
    }.toList

  private def findKing(board: Board, color: Color): Option[Pos] =
    board.allPieces.collectFirst {
      case (pos, Piece(c, PieceType.King)) if c == color => pos
    }

  private def isPassedPawn(pawn: Pos, color: Color, enemyPawns: List[Pos]): Boolean =
    enemyPawns.forall { enemy =>
      !adjacentFilesInclusive(pawn.file).contains(enemy.file) || !isAhead(enemy.rank, pawn.rank, color)
    }

  private def isBackwardPawn(pawn: Pos, color: Color, ownPawns: List[Pos], enemyPawns: List[Pos]): Boolean =
    val supportedByNeighbor =
      ownPawns.exists { other =>
        other != pawn &&
          adjacentFiles(pawn.file).contains(other.file) &&
          !isAhead(other.rank, pawn.rank, color)
      }
    val front = Pos(pawn.file, pawn.rank + pawnDirection(color))
    val frontControlledByEnemy =
      front.inBounds && enemyPawns.exists { enemy =>
        Math.abs(enemy.file - front.file) == 1 && enemy.rank + pawnDirection(color.other) == front.rank
      }
    !supportedByNeighbor && frontControlledByEnemy

  private def isAhead(candidateRank: Int, pawnRank: Int, color: Color): Boolean =
    if color == Color.White then candidateRank > pawnRank else candidateRank < pawnRank

  private def pawnDirection(color: Color): Int =
    if color == Color.White then 1 else -1

  private def adjacentFiles(file: Int): List[Int] =
    List(file - 1, file + 1).filter(file => file >= 0 && file < 8)

  private def adjacentFilesInclusive(file: Int): List[Int] =
    (file - 1 to file + 1).toList.filter(file => file >= 0 && file < 8)

  private def hasCastlingRight(game: Game, color: Color): Boolean =
    color match
      case Color.White => game.castlingRights.whiteKingside || game.castlingRights.whiteQueenside
      case Color.Black => game.castlingRights.blackKingside || game.castlingRights.blackQueenside

  private def pieceActivityScore(game: Game, color: Color, phase: Int): Int =
    val board = game.board
    val ownPieces = board.allPieces.collect { case (pos, piece) if piece.color == color => (pos, piece) }.toList
    val bishopPairBonus = if ownPieces.count(_._2.kind == PieceType.Bishop) >= 2 then 35 else 0
    val queenPenalty = earlyQueenPenalty(game, ownPieces, color, phase)

    ownPieces.map {
      case (pos, Piece(_, PieceType.Knight)) =>
        val rimPenalty = (if pos.file == 0 || pos.file == 7 then -18 else 0) + (if pos.rank == 0 || pos.rank == 7 then -18 else 0)
        val centerBonus =
          if pos.file >= 2 && pos.file <= 5 && pos.rank >= 2 && pos.rank <= 5 then 10 else 0
        rimPenalty + centerBonus
      case (pos, Piece(_, PieceType.Rook)) =>
        rookFileBonus(board, color, pos) + seventhRankBonus(pos, color, 18)
      case (pos, Piece(_, PieceType.Queen)) =>
        queenActivityBonus(game, color, pos, phase)
      case _ => 0
    }.sum + bishopPairBonus + queenPenalty

  private def earlyQueenPenalty(game: Game, ownPieces: List[(Pos, Piece)], color: Color, phase: Int): Int =
    if !isOpeningPhase(phase) then 0
    else
      val homeRank = if color == Color.White then 0 else 7
      val queenHome = Pos(3, homeRank)
      ownPieces.find(_._2.kind == PieceType.Queen) match
        case Some((queenPos, _)) if queenPos != queenHome =>
          val undevelopedMinors = ownPieces.count {
            case (pos, Piece(_, kind)) =>
              (kind == PieceType.Knight || kind == PieceType.Bishop) && pos.rank == homeRank
          }
          val uncastledPenalty = if !kingIsSafe(game, color) then 18 else 0
          -20 - undevelopedMinors * 12 - uncastledPenalty
        case _ => 0

  private def queenInvasionScore(game: Game, color: Color, phase: Int): Int =
    if !isOpeningPhase(phase) then 0
    else
      findQueen(game.board, color) match
        case Some(queen) if !kingIsSafe(game, color) =>
          val invadedEnemyHalf =
            color match
              case Color.White => queen.rank >= 4
              case Color.Black => queen.rank <= 3
          val farFromHomeFile = Math.abs(queen.file - 3)
          val invasionPenalty =
            if invadedEnemyHalf then 70 + farFromHomeFile * 6 else 0
          -invasionPenalty
        case _ => 0

  private def queenActivityBonus(game: Game, color: Color, queen: Pos, phase: Int): Int =
    if isOpeningPhase(phase) then 0
    else
      val centralBonus =
        if queen.file >= 2 && queen.file <= 5 && queen.rank >= 2 && queen.rank <= 5 then 18 else 0
      val safeInvasionBonus =
        if kingIsSafe(game, color) && isInEnemyHalf(queen, color) then 45 else 0
      seventhRankBonus(queen, color, 8) + centralBonus + safeInvasionBonus

  private def isInEnemyHalf(pos: Pos, color: Color): Boolean =
    color match
      case Color.White => pos.rank >= 4
      case Color.Black => pos.rank <= 3

  private def findQueen(board: Board, color: Color): Option[Pos] =
    board.allPieces.collectFirst {
      case (pos, Piece(c, PieceType.Queen)) if c == color => pos
    }

  private def kingIsSafe(game: Game, color: Color): Boolean =
    findKing(game.board, color).exists { king =>
      val homeRank = if color == Color.White then 0 else 7
      king.rank == homeRank && (king.file == 6 || king.file == 2)
    }

  private def isOpeningPhase(phase: Int): Boolean =
    phase > PestoTables.totalPhase * 2 / 3

  private def rookFileBonus(board: Board, color: Color, rook: Pos): Int =
    val ownPawnOnFile = board.allPieces.exists {
      case (pos, Piece(c, PieceType.Pawn)) => c == color && pos.file == rook.file
      case _ => false
    }
    val enemyPawnOnFile = board.allPieces.exists {
      case (pos, Piece(c, PieceType.Pawn)) => c == color.other && pos.file == rook.file
      case _ => false
    }

    if !ownPawnOnFile && !enemyPawnOnFile then 28
    else if !ownPawnOnFile && enemyPawnOnFile then 16
    else 0

  private def seventhRankBonus(pos: Pos, color: Color, bonus: Int): Int =
    val targetRank = if color == Color.White then 6 else 1
    if pos.rank == targetRank then bonus else 0

  private def mobilityScore(board: Board, color: Color, phase: Int): Int =
    board.allPieces.collect {
      case (pos, Piece(c, kind)) if c == color =>
        kind match
          case PieceType.Knight => knightMobility(board, color, pos) * 4
          case PieceType.Bishop => slidingMobility(board, color, pos, bishopDirections) * 3
          case PieceType.Rook   => slidingMobility(board, color, pos, rookDirections) * 2
          case PieceType.Queen  => if isOpeningPhase(phase) then 0 else slidingMobility(board, color, pos, queenDirections)
          case PieceType.Pawn   => pawnMobility(board, color, pos) * 2
          case PieceType.King   => 0
    }.sum

  private def knightMobility(board: Board, color: Color, pos: Pos): Int =
    knightOffsets.count { offset =>
      val to = pos + offset
      to.inBounds && !board.pieceAt(to).exists(_.color == color)
    }

  private def slidingMobility(board: Board, color: Color, pos: Pos, directions: List[(Int, Int)]): Int =
    directions.map(direction => rayMobility(board, color, pos, direction)).sum

  private def rayMobility(board: Board, color: Color, pos: Pos, direction: (Int, Int)): Int =
    var current = pos + direction
    var count = 0
    var blocked = false
    while current.inBounds && !blocked do
      board.pieceAt(current) match
        case None =>
          count += 1
          current = current + direction
        case Some(piece) =>
          if piece.color != color then count += 1
          blocked = true
    count

  private def pawnMobility(board: Board, color: Color, pos: Pos): Int =
    val dir = pawnDirection(color)
    val oneForward = pos + (0, dir)
    val quiet = if oneForward.inBounds && board.isEmpty(oneForward) then 1 else 0
    val captures =
      List(pos + (-1, dir), pos + (1, dir)).count { to =>
        to.inBounds && board.pieceAt(to).exists(_.color == color.other)
      }
    quiet + captures

  private val knightOffsets: List[(Int, Int)] =
    List((1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2))

  private val bishopDirections: List[(Int, Int)] =
    List((1, 1), (1, -1), (-1, 1), (-1, -1))

  private val rookDirections: List[(Int, Int)] =
    List((1, 0), (-1, 0), (0, 1), (0, -1))

  private val queenDirections: List[(Int, Int)] =
    bishopDirections ++ rookDirections

  private def hangingPiecesScore(board: Board, color: Color): Int =
    board.allPieces.collect {
      case (pos, piece) if piece.color == color && piece.kind != PieceType.King =>
        val attackers = attackersOf(board, pos, color.other)
        if attackers.isEmpty then 0
        else
          val defenders = attackersOf(board, pos, color)
          val leastAttacker = attackers.map(piece => pieceValue(piece.kind)).min
          val victim = pieceValue(piece.kind)
          val basePenalty =
            if defenders.isEmpty then victim / 3
            else if leastAttacker < victim then victim / 8
            else 0
          -Math.min(basePenalty, 300)
    }.sum

  private def attackersOf(board: Board, target: Pos, byColor: Color): List[Piece] =
    board.allPieces.collect {
      case (from, piece) if piece.color == byColor && attacksSquare(board, from, piece, target) => piece
    }.toList

  private def attacksSquare(board: Board, from: Pos, piece: Piece, target: Pos): Boolean =
    if from == target then false
    else
      val df = target.file - from.file
      val dr = target.rank - from.rank
      val absDf = Math.abs(df)
      val absDr = Math.abs(dr)

      piece.kind match
        case PieceType.King =>
          absDf <= 1 && absDr <= 1
        case PieceType.Queen =>
          ((df == 0 && dr != 0) || (dr == 0 && df != 0) || (absDf == absDr && df != 0)) && clearPath(board, from, target)
        case PieceType.Rook =>
          ((df == 0 && dr != 0) || (dr == 0 && df != 0)) && clearPath(board, from, target)
        case PieceType.Bishop =>
          absDf == absDr && df != 0 && clearPath(board, from, target)
        case PieceType.Knight =>
          (absDf == 2 && absDr == 1) || (absDf == 1 && absDr == 2)
        case PieceType.Pawn =>
          val dir = pawnDirection(piece.color)
          absDf == 1 && dr == dir

  private def clearPath(board: Board, from: Pos, to: Pos): Boolean =
    val df = to.file - from.file
    val dr = to.rank - from.rank
    val stepF = Integer.signum(df)
    val stepR = Integer.signum(dr)
    val steps = Math.max(Math.abs(df), Math.abs(dr)) - 1

    (1 to steps).forall { i =>
      board.isEmpty(Pos(from.file + stepF * i, from.rank + stepR * i))
    }

  // Used only for move ordering (MVV-LVA style) so we keep the simple centipawn values.
  private def pieceValue(kind: PieceType): Int = kind match
    case PieceType.Pawn   => 100
    case PieceType.Knight => 320
    case PieceType.Bishop => 330
    case PieceType.Rook   => 500
    case PieceType.Queen  => 900
    case PieceType.King   => 0

  private def orderMoves(game: Game, moves: List[Move], ttBestMove: Option[Move], ply: Int, context: SearchContext): List[Move] =
    moves.sortBy(mv => -moveOrderingScore(game, mv, ttBestMove, ply, context))

  private def moveOrderingScore(game: Game, mv: Move, ttBestMove: Option[Move], ply: Int, context: SearchContext): Int =
    if ttBestMove.contains(mv) then 100000000
    else
      val tacticalScore = captureUrgency(game, mv)
      val killerScore =
        context.killerMoves.get(ply) match
          case Some(first :: _) if first == mv => 70000000
          case Some(_ :: second :: _) if second == mv => 60000000
          case _ => 0

      val captureScore = if tacticalScore > 0 then 80000000 + tacticalScore * 1000 else 0
      val quietScore = if tacticalScore == 0 then context.historyScores.getOrElse(mv, 0) else 0
      captureScore + killerScore + quietScore

  private def captureUrgency(game: Game, mv: Move): Int =
    game.board.pieceAt(mv.from).map { moved =>
      val fromKind = moved.kind
      val target = game.board.pieceAt(mv.to)

      // MVV-LVA: Most Valuable Victim - Least Valuable Aggressor
      val captureValue =
        target match
          case Some(t) => pieceValue(t.kind) * 10 - pieceValue(fromKind)
          case None =>
            if fromKind == PieceType.Pawn && game.enPassantTarget.contains(mv.to) then pieceValue(PieceType.Pawn) * 10 - pieceValue(PieceType.Pawn)
            else 0

      val promotionValue =
        mv.promotion match
          case Some(role) =>
            val promotedKind = role.toPieceType
            pieceValue(promotedKind) - pieceValue(PieceType.Pawn)
          case None => 0

      captureValue + promotionValue
    }.getOrElse(0)

  private def recordCutoff(game: Game, mv: Move, depth: Int, ply: Int, context: SearchContext): Unit =
    if captureUrgency(game, mv) == 0 then
      val existing = context.killerMoves.getOrElse(ply, Nil).filterNot(_ == mv)
      context.killerMoves.update(ply, (mv :: existing).take(2))
      context.historyScores.update(mv, context.historyScores.getOrElse(mv, 0) + depth * depth)

  private def legalMovesProfiled(game: Game, context: SearchContext): List[Move] =
    if !context.profiler.enabled then game.legalMoves
    else
      val start = System.nanoTime()
      val result = game.legalMoves
      context.profiler.legalNanos += System.nanoTime() - start
      context.profiler.legalCalls += 1
      result

  private def applyMoveProfiled(game: Game, move: Move, context: SearchContext): Game =
    if !context.profiler.enabled then game.applyMove(move).toOption.get
    else
      val start = System.nanoTime()
      val result = game.applyMove(move).toOption.get
      context.profiler.applyMoveNanos += System.nanoTime() - start
      context.profiler.applyMoveCalls += 1
      result

  private def staticEvaluateProfiled(game: Game, context: SearchContext): Int =
    if !context.profiler.enabled then staticEvaluate(game)
    else
      val start = System.nanoTime()
      val result = staticEvaluate(game)
      context.profiler.evaluateNanos += System.nanoTime() - start
      context.profiler.evaluateCalls += 1
      result

  private def isInCheckProfiled(game: Game, context: SearchContext): Boolean =
    if !context.profiler.enabled then game.isInCheck
    else
      val start = System.nanoTime()
      val result = game.isInCheck
      context.profiler.inCheckNanos += System.nanoTime() - start
      context.profiler.inCheckCalls += 1
      result

  private def orderedMoves(game: Game, moves: List[Move], ttBestMove: Option[Move], ply: Int, context: SearchContext): List[Move] =
    if !context.profiler.enabled then orderMoves(game, moves, ttBestMove, ply, context)
    else
      val start = System.nanoTime()
      val result = orderMoves(game, moves, ttBestMove, ply, context)
      context.profiler.orderNanos += System.nanoTime() - start
      context.profiler.orderCalls += 1
      result

  private def transpositionKey(game: Game, context: SearchContext): Long =
    if !context.profiler.enabled then ZobristHash.hash(game)
    else
      val start = System.nanoTime()
      val result = ZobristHash.hash(game)
      context.profiler.hashNanos += System.nanoTime() - start
      context.profiler.hashCalls += 1
      result

  private def transpositionLookup(context: SearchContext, key: Long): Option[TranspositionEntry] =
    if !context.profiler.enabled then context.transpositionTable.get(key)
    else
      val start = System.nanoTime()
      val result = context.transpositionTable.get(key)
      context.profiler.ttNanos += System.nanoTime() - start
      context.profiler.ttCalls += 1
      result

  private def storeTransposition(
      context: SearchContext,
      key: Long,
      depth: Int,
      score: Int,
      bound: Bound,
      bestMove: Option[Move],
      ply: Int
  ): Unit =
    if context.transpositionTable.size >= maxTranspositionEntries then context.transpositionTable.clear()
    val storedScore = scoreToTransposition(score, ply)
    context.transpositionTable.get(key) match
      case Some(existing) if existing.depth > depth =>
      case _ => context.transpositionTable.update(key, TranspositionEntry(depth, storedScore, bound, bestMove))

  private def scoreToTransposition(score: Int, ply: Int): Int =
    if score >= mateScoreThreshold then score + ply
    else if score <= -mateScoreThreshold then score - ply
    else score

  private def scoreFromTransposition(score: Int, ply: Int): Int =
    if score >= mateScoreThreshold then score - ply
    else if score <= -mateScoreThreshold then score + ply
    else score

  private def boundFor(score: Int, alphaOrig: Int, beta: Int): Bound =
    if score <= alphaOrig then Bound.Upper
    else if score >= beta then Bound.Lower
    else Bound.Exact
