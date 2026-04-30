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

  override def chooseMove(state: AppState, remainingTimeMs: Option[Long] = None, incrementMs: Option[Long] = None): Future[Either[String, Move]] =
    val game = state.game
    val legal = game.legalMoves
    if legal.isEmpty then Future.successful(Left("No legal moves available."))
    else
      val budget = searchBudget(remainingTimeMs, incrementMs)

      val normalizedFen = Fen.encodeNormalized(game)
      
      val dbFuture = openingDb match
        case Some(db) => db.getMoves(normalizedFen)
        case None => Future.successful(List.empty)

      implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

      dbFuture.flatMap { dbMoves =>
        val validDbMoves = dbMoves.filter(m => legal.contains(m.move))
        rootSyzygyMove(game, legal).orElse(syzygyTransitionMove(game, legal)) match
          case Some(tablebaseMove) =>
            Future.successful(Right(tablebaseMove))
          case None if validDbMoves.nonEmpty =>
            // Pick the best known move from the database
            val bestDbMove = validDbMoves.maxBy(_.score).move
            Future.successful(Right(bestDbMove))
          case None =>
            searchMoveAsync(game, legal, budget)
      }

  private def rootSyzygyMove(game: Game, legal: List[Move]): Option[Move] =
    syzygyTablebase.flatMap(_.probe(game)).filter(result => legal.contains(result.bestMove)).map { result =>
      println(s"  syzygy | root ${result.label} | move ${result.bestMove}")
      result.bestMove
    }

  private def syzygyTransitionMove(game: Game, legal: List[Move]): Option[Move] =
    if game.board.allPieces.size != 6 then None
    else
      syzygyTablebase.flatMap { tablebase =>
        val winningTransitions =
          legal.flatMap { move =>
            game.applyMove(move).toOption.flatMap { next =>
              if !tablebase.canProbe(next) then None
              else
              tablebase.probe(next).flatMap { result =>
                val ourWdl = -result.wdl
                Option.when(ourWdl > 0)((move, result, transitionDtzScore(result.dtz)))
              }
            }
          }

        winningTransitions.sortBy(_._3).headOption.map { case (move, result, _) =>
          println(s"  syzygy | transition win via $move | after move ${result.label}")
          move
        }
      }

  private def transitionDtzScore(childDtz: Int): Int =
    Math.abs(childDtz)

  private def searchBudget(remainingTimeMs: Option[Long], incrementMs: Option[Long]): Long =
    remainingTimeMs match
      case Some(ms) =>
        if ms >= 24L * 60L * 60L * 1000L then thinkTimeMs
        else
          val increment = incrementMs.getOrElse(0L)
          val base = ms / 35
          val incrementBonus = increment * 8 / 10
          val raw = base + incrementBonus
          val normalCap = Math.max(500L, ms / 8)
          val panicReserve = if ms > 3000L then 1000L else 100L
          Math.max(300L, Math.min(raw, normalCap).min(Math.max(300L, ms - panicReserve)))
      case None => thinkTimeMs

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
          val (mv, score) = searchBestMoveWithAspiration(game, legal, currentDepth, bestScoreSoFar, deadline, context)
          val durationNanos = System.nanoTime() - startTime
          val durationMs = durationNanos / 1000000L
          val totalNodes = context.nodes.get()
          val depthNodes = totalNodes - nodesBeforeDepth
          val nps = if durationNanos > 0 then (depthNodes * 1000000000L) / durationNanos else 0

          bestMoveSoFar = mv
          bestScoreSoFar = score

          // Info-Logging für dich
          val scoreDesc =
            if score >= mateScoreThreshold then s"MATE+${(mateScore - score).max(0)}"
            else if score <= -mateScoreThreshold then s"MATE-${(mateScore + score).max(0)}"
            else s"${score / 100.0}"
          println(s"  depth $currentDepth | score $scoreDesc | move $mv | nodes $depthNodes/$totalNodes | nps $nps | tt ${context.transpositionTable.size} | ${durationMs}ms")

          // If we found a mate, no need to search deeper
          if score >= mateScore - 100 then throw new TimeLimitExceededException()
          currentDepth += 1
      catch
        case _: TimeLimitExceededException => // Search aborted, keep best move from last completed depth

      context.profiler.printSummary(System.nanoTime() - searchStartNanos, context.nodes.get(), context.transpositionTable.size)
      Right(bestMoveSoFar)
    }

  private def searchBestMoveWithAspiration(
      game: Game,
      legal: List[Move],
      depth: Int,
      previousScore: Int,
      deadline: Long,
      context: SearchContext
  ): (Move, Int) =
    if depth <= 1 || previousScore <= -mateScore / 2 then
      searchBestMove(game, legal, depth, -mateScore * 2, mateScore * 2, deadline, context)
    else
      var window = 50
      var alpha = previousScore - window
      var beta = previousScore + window
      var result = searchBestMove(game, legal, depth, alpha, beta, deadline, context)

      while (result._2 <= alpha || result._2 >= beta) && System.currentTimeMillis() < deadline do
        window *= 2
        alpha = Math.max(-mateScore * 2, previousScore - window)
        beta = Math.min(mateScore * 2, previousScore + window)
        result = searchBestMove(game, legal, depth, alpha, beta, deadline, context)

      result

  private def searchBestMove(game: Game, legal: List[Move], depth: Int, alpha0: Int, beta: Int, deadline: Long, context: SearchContext): (Move, Int) =
    var bestMove = legal.head
    var bestScore = -mateScore * 2

    var alpha = alpha0
    val alphaOrig = alpha0

    val positionKey = transpositionKey(game, context)
    val ttBestMove = transpositionLookup(context, positionKey).flatMap(_.bestMove)
    val ordered = orderedMoves(game, legal, ttBestMove, ply = 0, context)
    var i = 0
    while i < ordered.size && alpha < beta do
      if System.currentTimeMillis() > deadline then throw new TimeLimitExceededException()

      val mv = ordered(i)
      val next = applyMoveProfiled(game, mv, context)
      val score =
        if i == 0 then -negamax(next, depth - 1, -beta, -alpha, deadline, ply = 1, context)
        else
          var candidate = -negamax(next, depth - 1, -alpha - 1, -alpha, deadline, ply = 1, context)
          if candidate > alpha && candidate < beta then
            candidate = -negamax(next, depth - 1, -beta, -alpha, deadline, ply = 1, context)
          candidate

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
          val score =
            if i == 0 then -negamax(next, depth - 1, -beta0, -alpha, deadline, ply + 1, context)
            else
              var candidate = -negamax(next, depth - 1, -alpha - 1, -alpha, deadline, ply + 1, context)
              if candidate > alpha && candidate < beta0 then
                candidate = -negamax(next, depth - 1, -beta0, -alpha, deadline, ply + 1, context)
              candidate
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

    val captures = tacticalQuiescenceMoves(game, legalMovesProfiled(game, context), ply, context)
    val ordered = orderedMoves(game, captures, ttBestMove = None, ply, context)
    var i = 0
    while i < ordered.size && alpha < beta do
      val mv = ordered(i)
      val next = applyMoveProfiled(game, mv, context)
      val score = -quiescence(next, -beta, -alpha, deadline, ply + 1, context)
      if score > alpha then alpha = score
      i += 1

    alpha

  private def tacticalQuiescenceMoves(game: Game, legal: List[Move], ply: Int, context: SearchContext): List[Move] =
    legal.filter { move =>
      captureUrgency(game, move) > 0 || (ply <= 2 && givesCheck(game, move, context))
    }

  private def givesCheck(game: Game, move: Move, context: SearchContext): Boolean =
    val next = applyMoveProfiled(game, move, context)
    isInCheckProfiled(next, context)

  private val mateScore: Int = 100000
  private val mateScoreThreshold: Int = mateScore - 1000

  // ─── PeSTO Evaluation ────────────────────────────────────────────────────

  private final case class EvalContext(
      game: Game,
      board: Board,
      whitePieces: List[(Pos, Piece)],
      blackPieces: List[(Pos, Piece)],
      whitePawns: List[Pos],
      blackPawns: List[Pos],
      whiteKing: Option[Pos],
      blackKing: Option[Pos],
      whiteQueen: Option[Pos],
      blackQueen: Option[Pos],
      phase: Int
  ):
    def piecesOf(color: Color): List[(Pos, Piece)] =
      if color == Color.White then whitePieces else blackPieces

    def pawnsOf(color: Color): List[Pos] =
      if color == Color.White then whitePawns else blackPawns

    def kingOf(color: Color): Option[Pos] =
      if color == Color.White then whiteKing else blackKing

    def queenOf(color: Color): Option[Pos] =
      if color == Color.White then whiteQueen else blackQueen

  private object EvalContext:
    def from(game: Game): EvalContext =
      val pieces = game.board.allPieces.toList
      val whitePieces = pieces.collect { case entry @ (_, Piece(Color.White, _)) => entry }
      val blackPieces = pieces.collect { case entry @ (_, Piece(Color.Black, _)) => entry }
      val whitePawns = whitePieces.collect { case (pos, Piece(_, PieceType.Pawn)) => pos }
      val blackPawns = blackPieces.collect { case (pos, Piece(_, PieceType.Pawn)) => pos }
      val whiteKing = whitePieces.collectFirst { case (pos, Piece(_, PieceType.King)) => pos }
      val blackKing = blackPieces.collectFirst { case (pos, Piece(_, PieceType.King)) => pos }
      val whiteQueen = whitePieces.collectFirst { case (pos, Piece(_, PieceType.Queen)) => pos }
      val blackQueen = blackPieces.collectFirst { case (pos, Piece(_, PieceType.Queen)) => pos }
      val phase =
        pieces.foldLeft(0) { case (acc, (_, piece)) =>
          acc + PestoTables.phaseWeight.getOrElse(piece.kind, 0)
        }

      EvalContext(
        game,
        game.board,
        whitePieces,
        blackPieces,
        whitePawns,
        blackPawns,
        whiteKing,
        blackKing,
        whiteQueen,
        blackQueen,
        Math.min(phase, PestoTables.totalPhase)
      )

  /**
   * Score all pieces for one color using PeSTO material + PST values,
   * interpolated between middlegame and endgame.
   */
  private def colorScore(pieces: List[(Pos, Piece)], isWhite: Boolean, phase: Int): Int =
    pieces.map { case (pos, piece) =>
      PestoTables.pieceScore(piece.kind, pos, isWhite, phase)
    }.sum

  private def staticEvaluate(game: Game): Int =
    val eval = EvalContext.from(game)
    val phase = eval.phase
    val whiteScore = colorScore(eval.whitePieces, isWhite = true,  phase)
    val blackScore = colorScore(eval.blackPieces, isWhite = false, phase)
    val diff =
      whiteScore - blackScore +
        pawnStructureScore(eval, Color.White) - pawnStructureScore(eval, Color.Black) +
        kingSafetyScore(eval, Color.White) - kingSafetyScore(eval, Color.Black) +
        pieceActivityScore(eval, Color.White) - pieceActivityScore(eval, Color.Black) +
        queenInvasionScore(eval, Color.White) - queenInvasionScore(eval, Color.Black) +
        mobilityScore(eval, Color.White) - mobilityScore(eval, Color.Black) +
        hangingPiecesScore(eval, Color.White) - hangingPiecesScore(eval, Color.Black)
    game.sideToMove match
      case Color.White => diff
      case Color.Black => -diff

  private def pawnStructureScore(eval: EvalContext, color: Color): Int =
    val ownPawns = eval.pawnsOf(color)
    val enemyPawns = eval.pawnsOf(color.other)
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

  private def kingSafetyScore(eval: EvalContext, color: Color): Int =
    eval.kingOf(color) match
      case None => 0
      case Some(king) =>
        val homeRank = if color == Color.White then 0 else 7
        val castledBonus =
          if king.rank == homeRank && (king.file == 6 || king.file == 2) then 35
          else if king.rank == homeRank && king.file == 4 && hasCastlingRight(eval.game, color) then 10
          else if eval.phase > PestoTables.totalPhase / 2 then -20
          else 0

        val shieldScore = pawnShieldScore(eval.board, color, king)
        val openFilePenalty = kingFileExposurePenalty(eval, color, king)
        val dangerPenalty = kingAttackDangerPenalty(eval, color, king)
        castledBonus + shieldScore - openFilePenalty - dangerPenalty

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

  private def kingFileExposurePenalty(eval: EvalContext, color: Color, king: Pos): Int =
    adjacentFilesInclusive(king.file).map { file =>
      val ownPawnOnFile = eval.pawnsOf(color).exists(_.file == file)
      val enemyHeavyOnFile = eval.piecesOf(color.other).exists {
        case (pos, Piece(_, kind)) =>
          pos.file == file && (kind == PieceType.Rook || kind == PieceType.Queen)
      }

      val openPenalty = if ownPawnOnFile then 0 else 14
      val heavyPenalty = if !ownPawnOnFile && enemyHeavyOnFile then 18 else 0
      openPenalty + heavyPenalty
    }.sum

  private def kingAttackDangerPenalty(eval: EvalContext, color: Color, king: Pos): Int =
    val enemyPieces = eval.piecesOf(color.other)
    val zone = kingZone(king)
    val attackedZoneSquares =
      zone.count(square => enemyPieces.exists { case (from, piece) => attacksSquare(eval.board, from, piece, square) })

    val closeEnemyQueenPenalty =
      eval.queenOf(color.other) match
        case Some(queen) =>
          val distance = Math.max(Math.abs(queen.file - king.file), Math.abs(queen.rank - king.rank))
          if distance <= 2 then 65
          else if distance <= 3 then 30
          else 0
        case None => 0

    val closeMinorPenalty = enemyPieces.collect {
      case (pos, Piece(_, kind)) if kind == PieceType.Knight || kind == PieceType.Bishop =>
        val distance = Math.max(Math.abs(pos.file - king.file), Math.abs(pos.rank - king.rank))
        if distance <= 2 then 18 else 0
    }.sum

    val openLinePenalty = kingLinePressurePenalty(eval, color, king)
    attackedZoneSquares * 12 + closeEnemyQueenPenalty + closeMinorPenalty + openLinePenalty

  private def kingZone(king: Pos): List[Pos] =
    (for
      df <- -1 to 1
      dr <- -1 to 1
      pos = Pos(king.file + df, king.rank + dr)
      if pos.inBounds
    yield pos).toList

  private def kingLinePressurePenalty(eval: EvalContext, color: Color, king: Pos): Int =
    val heavyPieces = eval.piecesOf(color.other).collect {
      case entry @ (_, Piece(_, kind)) if kind == PieceType.Rook || kind == PieceType.Queen => entry
    }
    heavyPieces.map {
      case (pos, Piece(_, kind)) =>
        val sameFile = pos.file == king.file
        val sameRank = pos.rank == king.rank
        val sameDiagonal = Math.abs(pos.file - king.file) == Math.abs(pos.rank - king.rank)
        if (sameFile || sameRank || sameDiagonal) && clearPath(eval.board, pos, king) then
          if kind == PieceType.Queen then 35 else 22
        else 0
    }.sum

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

  private def pieceActivityScore(eval: EvalContext, color: Color): Int =
    val board = eval.board
    val phase = eval.phase
    val ownPieces = eval.piecesOf(color)
    val bishopPairBonus = if ownPieces.count(_._2.kind == PieceType.Bishop) >= 2 then 35 else 0
    val queenPenalty = earlyQueenPenalty(eval, ownPieces, color)

    ownPieces.map {
      case (pos, Piece(_, PieceType.Knight)) =>
        val rimPenalty = (if pos.file == 0 || pos.file == 7 then -18 else 0) + (if pos.rank == 0 || pos.rank == 7 then -18 else 0)
        val centerBonus =
          if pos.file >= 2 && pos.file <= 5 && pos.rank >= 2 && pos.rank <= 5 then 10 else 0
        rimPenalty + centerBonus
      case (pos, Piece(_, PieceType.Rook)) =>
        rookFileBonus(eval, color, pos) + seventhRankBonus(pos, color, 18)
      case (pos, Piece(_, PieceType.Queen)) =>
        queenActivityBonus(eval, color, pos)
      case _ => 0
    }.sum + bishopPairBonus + queenPenalty

  private def earlyQueenPenalty(eval: EvalContext, ownPieces: List[(Pos, Piece)], color: Color): Int =
    if !isOpeningPhase(eval.phase) then 0
    else
      val homeRank = if color == Color.White then 0 else 7
      val queenHome = Pos(3, homeRank)
      ownPieces.find(_._2.kind == PieceType.Queen) match
        case Some((queenPos, _)) if queenPos != queenHome =>
          val undevelopedMinors = ownPieces.count {
            case (pos, Piece(_, kind)) =>
              (kind == PieceType.Knight || kind == PieceType.Bishop) && pos.rank == homeRank
          }
          val uncastledPenalty = if !kingIsSafe(eval, color) then 18 else 0
          -20 - undevelopedMinors * 12 - uncastledPenalty
        case _ => 0

  private def queenInvasionScore(eval: EvalContext, color: Color): Int =
    if !isOpeningPhase(eval.phase) then 0
    else
      eval.queenOf(color) match
        case Some(queen) if !kingIsSafe(eval, color) =>
          val invadedEnemyHalf =
            color match
              case Color.White => queen.rank >= 4
              case Color.Black => queen.rank <= 3
          val farFromHomeFile = Math.abs(queen.file - 3)
          val invasionPenalty =
            if invadedEnemyHalf then 70 + farFromHomeFile * 6 else 0
          -invasionPenalty
        case _ => 0

  private def queenActivityBonus(eval: EvalContext, color: Color, queen: Pos): Int =
    if isOpeningPhase(eval.phase) then 0
    else
      val centralBonus =
        if queen.file >= 2 && queen.file <= 5 && queen.rank >= 2 && queen.rank <= 5 then 18 else 0
      val safeInvasionBonus =
        if kingIsSafe(eval, color) && isInEnemyHalf(queen, color) then 45 else 0
      seventhRankBonus(queen, color, 8) + centralBonus + safeInvasionBonus

  private def isInEnemyHalf(pos: Pos, color: Color): Boolean =
    color match
      case Color.White => pos.rank >= 4
      case Color.Black => pos.rank <= 3

  private def kingIsSafe(eval: EvalContext, color: Color): Boolean =
    eval.kingOf(color).exists { king =>
      val homeRank = if color == Color.White then 0 else 7
      king.rank == homeRank && (king.file == 6 || king.file == 2)
    }

  private def isOpeningPhase(phase: Int): Boolean =
    phase > PestoTables.totalPhase * 2 / 3

  private def rookFileBonus(eval: EvalContext, color: Color, rook: Pos): Int =
    val ownPawnOnFile = eval.pawnsOf(color).exists(_.file == rook.file)
    val enemyPawnOnFile = eval.pawnsOf(color.other).exists(_.file == rook.file)

    if !ownPawnOnFile && !enemyPawnOnFile then 28
    else if !ownPawnOnFile && enemyPawnOnFile then 16
    else 0

  private def seventhRankBonus(pos: Pos, color: Color, bonus: Int): Int =
    val targetRank = if color == Color.White then 6 else 1
    if pos.rank == targetRank then bonus else 0

  private def mobilityScore(eval: EvalContext, color: Color): Int =
    val board = eval.board
    eval.piecesOf(color).map {
      case (pos, Piece(_, kind)) =>
        kind match
          case PieceType.Knight => knightMobility(board, color, pos) * 4
          case PieceType.Bishop => slidingMobility(board, color, pos, bishopDirections) * 3
          case PieceType.Rook   => slidingMobility(board, color, pos, rookDirections) * 2
          case PieceType.Queen  => if isOpeningPhase(eval.phase) then 0 else slidingMobility(board, color, pos, queenDirections)
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

  private def hangingPiecesScore(eval: EvalContext, color: Color): Int =
    val board = eval.board
    val attackers = eval.piecesOf(color.other)
    val defenders = eval.piecesOf(color)
    defenders.collect {
      case (pos, piece) if piece.kind != PieceType.King =>
        leastAttackerValue(board, attackers, pos) match
          case None => 0
          case Some(leastAttacker) =>
            val defended = isAttackedBy(board, defenders, pos)
            val victim = pieceValue(piece.kind)
            val basePenalty =
              if !defended then victim / 3
              else if leastAttacker < victim then victim / 8
              else 0
            -Math.min(basePenalty, 300)
    }.sum

  private def leastAttackerValue(board: Board, candidates: List[(Pos, Piece)], target: Pos): Option[Int] =
    var best = Int.MaxValue
    var found = false
    candidates.foreach { case (from, piece) =>
      if attacksSquare(board, from, piece, target) then
        best = Math.min(best, pieceValue(piece.kind))
        found = true
    }
    if found then Some(best) else None

  private def isAttackedBy(board: Board, candidates: List[(Pos, Piece)], target: Pos): Boolean =
    candidates.exists { case (from, piece) =>
      attacksSquare(board, from, piece, target)
    }

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
