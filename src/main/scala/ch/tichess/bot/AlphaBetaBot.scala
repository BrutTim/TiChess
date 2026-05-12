package ch.tichess.bot

import ch.tichess.controller.AppState
import ch.tichess.model.{BitboardAttacks, Bitboards, Board, Color, Game, Move, Piece, PieceType, Pos}

import scala.concurrent.Future
import scala.collection.mutable
import java.util.concurrent.atomic.AtomicBoolean
import ch.tichess.model.Fen

/**
 * Simple alpha-beta bot with a lightweight material-based heuristic.
 *
 * Note: this is intended as an MVP and therefore keeps the evaluation cheap.
 */
class AlphaBetaBot(thinkTimeMs: Long = 10000L, openingDb: Option[OpeningDatabase] = None) extends ChessBot:
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
    var nullMoveAttempts: Long = 0L
    var nullMoveCutoffs: Long = 0L
    var evalContextNanos: Long = 0L
    var evalContextCalls: Long = 0L
    var evalMaterialNanos: Long = 0L
    var evalMaterialCalls: Long = 0L
    var evalPawnNanos: Long = 0L
    var evalPawnCalls: Long = 0L
    var evalKingNanos: Long = 0L
    var evalKingCalls: Long = 0L
    var evalActivityNanos: Long = 0L
    var evalActivityCalls: Long = 0L
    var evalQueenNanos: Long = 0L
    var evalQueenCalls: Long = 0L
    var evalMobilityNanos: Long = 0L
    var evalMobilityCalls: Long = 0L
    var evalHangingNanos: Long = 0L
    var evalHangingCalls: Long = 0L
    var evalAttackNanos: Long = 0L
    var evalAttackCalls: Long = 0L
    var evalClearPathNanos: Long = 0L
    var evalClearPathCalls: Long = 0L

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
      nullMoveAttempts = 0L
      nullMoveCutoffs = 0L
      evalContextNanos = 0L
      evalContextCalls = 0L
      evalMaterialNanos = 0L
      evalMaterialCalls = 0L
      evalPawnNanos = 0L
      evalPawnCalls = 0L
      evalKingNanos = 0L
      evalKingCalls = 0L
      evalActivityNanos = 0L
      evalActivityCalls = 0L
      evalQueenNanos = 0L
      evalQueenCalls = 0L
      evalMobilityNanos = 0L
      evalMobilityCalls = 0L
      evalHangingNanos = 0L
      evalHangingCalls = 0L
      evalAttackNanos = 0L
      evalAttackCalls = 0L
      evalClearPathNanos = 0L
      evalClearPathCalls = 0L

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
        println(s"    null   attempts $nullMoveAttempts | cutoffs $nullMoveCutoffs")
        if evaluateCalls > 0 then
          println("    eval breakdown")
          printLine("      ctx", evalContextNanos, evalContextCalls, evaluateNanos)
          printLine("      mat", evalMaterialNanos, evalMaterialCalls, evaluateNanos)
          printLine("      pawn", evalPawnNanos, evalPawnCalls, evaluateNanos)
          printLine("      king", evalKingNanos, evalKingCalls, evaluateNanos)
          printLine("      act", evalActivityNanos, evalActivityCalls, evaluateNanos)
          printLine("      queen", evalQueenNanos, evalQueenCalls, evaluateNanos)
          printLine("      mob", evalMobilityNanos, evalMobilityCalls, evaluateNanos)
          printLine("      hang", evalHangingNanos, evalHangingCalls, evaluateNanos)
          printLine("      attack", evalAttackNanos, evalAttackCalls, evaluateNanos)
          printLine("      path", evalClearPathNanos, evalClearPathCalls, evaluateNanos)

    def recordEvalContext(nanos: Long): Unit =
      evalContextNanos += nanos
      evalContextCalls += 1

    def recordEvalMaterial(nanos: Long): Unit =
      evalMaterialNanos += nanos
      evalMaterialCalls += 1

    def recordEvalPawn(nanos: Long): Unit =
      evalPawnNanos += nanos
      evalPawnCalls += 1

    def recordEvalKing(nanos: Long): Unit =
      evalKingNanos += nanos
      evalKingCalls += 1

    def recordEvalActivity(nanos: Long): Unit =
      evalActivityNanos += nanos
      evalActivityCalls += 1

    def recordEvalQueen(nanos: Long): Unit =
      evalQueenNanos += nanos
      evalQueenCalls += 1

    def recordEvalMobility(nanos: Long): Unit =
      evalMobilityNanos += nanos
      evalMobilityCalls += 1

    def recordEvalHanging(nanos: Long): Unit =
      evalHangingNanos += nanos
      evalHangingCalls += 1

    def recordEvalAttack(nanos: Long): Unit =
      evalAttackNanos += nanos
      evalAttackCalls += 1

    def recordEvalClearPath(nanos: Long): Unit =
      evalClearPathNanos += nanos
      evalClearPathCalls += 1

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
    var rootRepetitionCounts: Map[String, Int] = Map.empty
    var rootSideToMove: Color = Color.White
    var rootDrawContempt: Int = 0
    var stopRequested: () => Boolean = () => false
    val pathRepetitionCounts: mutable.HashMap[String, Int] = mutable.HashMap.empty
    val nodes = java.util.concurrent.atomic.AtomicLong(0)
    val profiler = SearchProfiler()

  private val context = SearchContext()
  private val searchLock = new Object
  private val ponderCancel = AtomicBoolean(false)
  private val maxTranspositionEntries = 1000000
  private val transpositionTrimTarget = maxTranspositionEntries * 7 / 10
  private val syzygyTablebase = SyzygyTablebase.fromEnv()

  override def chooseMove(state: AppState, remainingTimeMs: Option[Long] = None, incrementMs: Option[Long] = None): Future[Either[String, Move]] =
    ponderCancel.set(true)
    val game = state.game
    val legal = game.legalMoves
    if legal.isEmpty then Future.successful(Left("No legal moves available."))
    else
      val legalForDecision = avoidRootThreefold(state, legal)
      val budget = searchBudget(remainingTimeMs, incrementMs)

      val normalizedFen = Fen.encodeNormalized(game)
      
      val dbFuture = openingDb match
        case Some(db) => db.getMoves(normalizedFen)
        case None => Future.successful(List.empty)

      implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

      dbFuture.flatMap { dbMoves =>
        val validDbMoves = dbMoves.filter(m => legalForDecision.contains(m.move))
        rootSyzygyMove(game, legalForDecision).orElse(syzygyTransitionMove(game, legalForDecision)) match
          case Some(tablebaseMove) =>
            Future.successful(Right(tablebaseMove))
          case None if validDbMoves.nonEmpty =>
            // Pick the best known move from the database
            val bestDbMove = validDbMoves.maxBy(_.score).move
            Future.successful(Right(bestDbMove))
          case None =>
            searchMoveAsync(game, legalForDecision, budget, repetitionCounts(state.startGame, state.moveHistory))
      }

  override def ponder(state: AppState, maxWarmupMs: Long): Future[Unit] =
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val budget = Math.min(maxWarmupMs, 5000L).max(0L)
    val game = state.game
    val legal = game.legalMoves

    if budget <= 0L || legal.isEmpty || game.isCheckmate || game.isDraw then Future.unit
    else
      ponderCancel.set(false)
      val legalForDecision = avoidRootThreefold(state, legal)
      searchMoveAsync(
        game,
        legalForDecision,
        budget,
        repetitionCounts(state.startGame, state.moveHistory),
        log = false,
        stopRequested = () => ponderCancel.get()
      ).map(_ => ()).recover { case _ => () }

  private def avoidRootThreefold(state: AppState, legal: List[Move]): List[Move] =
    if state.moveHistory.isEmpty then legal
    else
      val counts = repetitionCounts(state.startGame, state.moveHistory)
      val directRepetitionMoves = legal.filter { move =>
        state.game.applyMove(move).toOption.exists { next =>
          isImmediateThreefold(next, counts)
        }
      }
      val shouldAvoidDraw = drawContemptFor(staticEvaluate(state.game)) > 0

      val opponentRepetitionMoves =
        if shouldAvoidDraw then
          legal.filterNot(directRepetitionMoves.toSet).filter { move =>
            state.game.applyMove(move).toOption.exists { next =>
              next.legalMoves.exists { reply =>
                next.applyMove(reply).toOption.exists(afterReply => isImmediateThreefold(afterReply, counts))
              }
            }
          }
        else Nil

      val drawMoves = (directRepetitionMoves ++ opponentRepetitionMoves).distinct
      val alternatives = legal.filterNot(drawMoves.toSet)

      if drawMoves.nonEmpty && alternatives.nonEmpty && shouldAvoidDraw then
        val opponentSuffix =
          if opponentRepetitionMoves.nonEmpty then s", ${opponentRepetitionMoves.size} allowing opponent claim" else ""
        println(s"  draw-avoid | skipping ${drawMoves.size} drawish root move(s) (${directRepetitionMoves.size} direct$opponentSuffix)")
        alternatives
      else legal

  private def isImmediateThreefold(game: Game, counts: Map[String, Int]): Boolean =
    counts.getOrElse(Fen.encodeNormalized(game), 0) + 1 >= 3

  private def repetitionCounts(startGame: Game, moves: Vector[Move]): Map[String, Int] =
    var game = startGame
    val counts = mutable.HashMap[String, Int](Fen.encodeNormalized(game) -> 1)
    moves.foreach { move =>
      game.applyMove(move).toOption.foreach { next =>
        game = next
        val key = Fen.encodeNormalized(game)
        counts.update(key, counts.getOrElse(key, 0) + 1)
      }
    }
    counts.toMap

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

  private def searchMoveAsync(
      game: Game,
      legal: List[Move],
      budget: Long,
      repetitions: Map[String, Int],
      log: Boolean = true,
      stopRequested: () => Boolean = () => false
  ): Future[Either[String, Move]] =
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    // Execute CPU-heavy search in a global thread pool
    Future {
      searchLock.synchronized {
      val deadline = System.currentTimeMillis() + budget
      val searchStartNanos = System.nanoTime()
      var bestMoveSoFar = legal.head
      var bestScoreSoFar = -mateScore
      var currentDepth = 1
      context.nodes.set(0) // Reset node counter for this specific move display
      context.profiler.reset()
      context.rootRepetitionCounts = repetitions
      context.rootSideToMove = game.sideToMove
      context.rootDrawContempt = drawContemptFor(staticEvaluate(game))
      context.stopRequested = stopRequested
      context.pathRepetitionCounts.clear()

      try
        // Iterative Deepening
        while !searchStopped(deadline, context) do
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
          if log then
            println(s"  depth $currentDepth | score $scoreDesc | move $mv | nodes $depthNodes/$totalNodes | nps $nps | tt ${context.transpositionTable.size} | ${durationMs}ms")

          // If we found a mate, no need to search deeper
          if score >= mateScore - 100 then throw new TimeLimitExceededException()
          currentDepth += 1
      catch
        case _: TimeLimitExceededException => // Search aborted, keep best move from last completed depth

      if log then context.profiler.printSummary(System.nanoTime() - searchStartNanos, context.nodes.get(), context.transpositionTable.size)
      context.stopRequested = () => false
      Right(bestMoveSoFar)
      }
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

      while (result._2 <= alpha || result._2 >= beta) && !searchStopped(deadline, context) do
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
      if searchStopped(deadline, context) then throw new TimeLimitExceededException()

      val mv = ordered(i)
      val next = applyMoveProfiled(game, mv, context)
      val score =
        if i == 0 then -withRepetition(next, context)(negamax(next, depth - 1, -beta, -alpha, deadline, ply = 1, context))
        else
          var candidate = -withRepetition(next, context)(negamax(next, depth - 1, -alpha - 1, -alpha, deadline, ply = 1, context))
          if candidate > alpha && candidate < beta then
            candidate = -withRepetition(next, context)(negamax(next, depth - 1, -beta, -alpha, deadline, ply = 1, context))
          candidate

      if score > bestScore then
        bestScore = score
        bestMove = mv

      alpha = Math.max(alpha, bestScore)
      i += 1

    storeTransposition(context, positionKey, depth, bestScore, boundFor(bestScore, alphaOrig, beta), Some(bestMove), ply = 0)
    (bestMove, bestScore)

  private def negamax(
      game: Game,
      depth: Int,
      alpha0: Int,
      beta0: Int,
      deadline: Long,
      ply: Int,
      context: SearchContext,
      allowNullMove: Boolean = true
  ): Int =
    context.nodes.incrementAndGet()
    if searchStopped(deadline, context) then throw new TimeLimitExceededException()

    if isRepetitionDraw(game, context) || game.halfMoveClock >= 100 then return drawScoreFor(game, context)

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

      val inCheck = isInCheckProfiled(game, context)
      if shouldTryNullMove(game, depth, ply, inCheck, allowNullMove) then
        context.profiler.nullMoveAttempts += 1
        val reduction = nullMoveReduction(depth)
        val nullGame = game.copy(
          sideToMove = game.sideToMove.other,
          enPassantTarget = None,
          halfMoveClock = game.halfMoveClock + 1
        )
        val nullScore = -negamax(
          nullGame,
          depth - 1 - reduction,
          -beta0,
          -beta0 + 1,
          deadline,
          ply + 1,
          context,
          allowNullMove = false
        )
        if nullScore >= beta0 then
          context.profiler.nullMoveCutoffs += 1
          return beta0

      val legal = legalMovesProfiled(game, context)
      if legal.isEmpty then
        if inCheck then -mateScore + ply
        else drawScoreFor(game, context)
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
            if i == 0 then -withRepetition(next, context)(negamax(next, depth - 1, -beta0, -alpha, deadline, ply + 1, context))
            else
              val reduction = lateMoveReduction(game, mv, depth, i, inCheck, ttBestMove)
              var candidate = -withRepetition(next, context)(negamax(next, depth - 1 - reduction, -alpha - 1, -alpha, deadline, ply + 1, context))
              if reduction > 0 && candidate > alpha then
                candidate = -withRepetition(next, context)(negamax(next, depth - 1, -alpha - 1, -alpha, deadline, ply + 1, context))
              if candidate > alpha && candidate < beta0 then
                candidate = -withRepetition(next, context)(negamax(next, depth - 1, -beta0, -alpha, deadline, ply + 1, context))
              candidate
          if score > best then
            best = score
            bestMove = Some(mv)
          alpha = Math.max(alpha, best)
          if alpha >= beta0 then recordCutoff(game, mv, depth, ply, context)
          i += 1

        storeTransposition(context, positionKey, depth, best, boundFor(best, alphaOrig, beta0), bestMove, ply)
        best

  private def shouldTryNullMove(game: Game, depth: Int, ply: Int, inCheck: Boolean, allowNullMove: Boolean): Boolean =
    allowNullMove &&
      ply > 0 &&
      depth >= 3 &&
      !inCheck &&
      farFromFiftyMoveDraw(game) &&
      hasNullMoveMaterial(game, game.sideToMove)

  private def farFromFiftyMoveDraw(game: Game): Boolean =
    game.halfMoveClock < 95

  private def hasNullMoveMaterial(game: Game, color: Color): Boolean =
    val bitboards = game.board.bitboards
    bitboards.queens(color) != 0L ||
      bitboards.rooks(color) != 0L ||
      Bitboards.popCount(bitboards.bishops(color) | bitboards.knights(color)) >= 2

  private def nullMoveReduction(depth: Int): Int =
    if depth >= 6 then 3 else 2

  private def lateMoveReduction(
      game: Game,
      move: Move,
      depth: Int,
      moveIndex: Int,
      inCheck: Boolean,
      ttBestMove: Option[Move]
  ): Int =
    if depth < 3 then 0
    else if moveIndex < 4 then 0
    else if inCheck then 0
    else if ttBestMove.contains(move) then 0
    else if captureUrgency(game, move) > 0 then 0
    else if move.promotion.nonEmpty then 0
    else if depth >= 5 && moveIndex >= 8 then 2
    else 1

  private def searchCheckEvasions(game: Game, legal: List[Move], alpha0: Int, beta: Int, deadline: Long, ply: Int, context: SearchContext): Int =
    var alpha = alpha0
    var best = -mateScore * 2
    val ordered = orderedMoves(game, legal, ttBestMove = None, ply, context)
    var i = 0
    while i < ordered.size && alpha < beta do
      if searchStopped(deadline, context) then throw new TimeLimitExceededException()
      val next = applyMoveProfiled(game, ordered(i), context)
      val score = -withRepetition(next, context)(quiescence(next, -beta, -alpha, deadline, ply + 1, context))
      best = Math.max(best, score)
      alpha = Math.max(alpha, best)
      i += 1
    best

  private def quiescence(game: Game, alpha0: Int, beta: Int, deadline: Long, ply: Int, context: SearchContext): Int =
    context.nodes.incrementAndGet()
    if searchStopped(deadline, context) then throw new TimeLimitExceededException()
    if isRepetitionDraw(game, context) || game.halfMoveClock >= 100 then return drawScoreFor(game, context)

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
      val score = -withRepetition(next, context)(quiescence(next, -beta, -alpha, deadline, ply + 1, context))
      if score > alpha then alpha = score
      i += 1

    alpha

  private def searchStopped(deadline: Long, context: SearchContext): Boolean =
    context.stopRequested() || System.currentTimeMillis() > deadline

  private def tacticalQuiescenceMoves(game: Game, legal: List[Move], ply: Int, context: SearchContext): List[Move] =
    legal.filter { move =>
      captureUrgency(game, move) > 0 || (ply <= 2 && givesCheck(game, move, context))
    }

  private def givesCheck(game: Game, move: Move, context: SearchContext): Boolean =
    val next = applyMoveProfiled(game, move, context)
    isInCheckProfiled(next, context)

  private val mateScore: Int = 100000
  private val mateScoreThreshold: Int = mateScore - 1000
  private def drawContemptFor(rootScore: Int): Int =
    if rootScore >= 250 then 35
    else if rootScore >= -100 then 20
    else 0

  private def drawScoreFor(game: Game, context: SearchContext): Int =
    if context.rootDrawContempt == 0 then 0
    else if game.sideToMove == context.rootSideToMove then -context.rootDrawContempt
    else context.rootDrawContempt

  private def isRepetitionDraw(game: Game, context: SearchContext): Boolean =
    val key = Fen.encodeNormalized(game)
    context.rootRepetitionCounts.getOrElse(key, 0) + context.pathRepetitionCounts.getOrElse(key, 0) >= 3

  private def withRepetition(game: Game, context: SearchContext)(score: => Int): Int =
    val key = Fen.encodeNormalized(game)
    context.pathRepetitionCounts.update(key, context.pathRepetitionCounts.getOrElse(key, 0) + 1)
    try score
    finally
      val nextCount = context.pathRepetitionCounts(key) - 1
      if nextCount == 0 then context.pathRepetitionCounts.remove(key)
      else context.pathRepetitionCounts.update(key, nextCount)

  // ─── PeSTO Evaluation ────────────────────────────────────────────────────

  private final case class EvalContext(
      game: Game,
      board: Board,
      bitboards: Bitboards,
      whiteAttacks: AttackInfo,
      blackAttacks: AttackInfo,
      whitePieces: List[(Pos, Piece)],
      blackPieces: List[(Pos, Piece)],
      whitePawns: List[Pos],
      blackPawns: List[Pos],
      whiteKing: Option[Pos],
      blackKing: Option[Pos],
      whiteQueen: Option[Pos],
      blackQueen: Option[Pos],
      phase: Int,
      profiler: Option[SearchProfiler]
  ):
    def attacksOf(color: Color): AttackInfo =
      if color == Color.White then whiteAttacks else blackAttacks

    def piecesOf(color: Color): List[(Pos, Piece)] =
      if color == Color.White then whitePieces else blackPieces

    def pawnsOf(color: Color): List[Pos] =
      if color == Color.White then whitePawns else blackPawns

    def kingOf(color: Color): Option[Pos] =
      if color == Color.White then whiteKing else blackKing

    def queenOf(color: Color): Option[Pos] =
      if color == Color.White then whiteQueen else blackQueen

  private final case class AttackInfo(
      pawns: Long,
      knights: Long,
      bishops: Long,
      rooks: Long,
      queens: Long,
      kings: Long
  ):
    def all: Long = pawns | knights | bishops | rooks | queens | kings

    def byKind(kind: PieceType): Long =
      kind match
        case PieceType.Pawn   => pawns
        case PieceType.Knight => knights
        case PieceType.Bishop => bishops
        case PieceType.Rook   => rooks
        case PieceType.Queen  => queens
        case PieceType.King   => kings

  private object AttackInfo:
    def from(bitboards: Bitboards, color: Color): AttackInfo =
      var pawns = 0L
      var knights = 0L
      var bishops = 0L
      var rooks = 0L
      var queens = 0L
      var kings = 0L

      Bitboards.foreachSetBit(bitboards.pawns(color)) { square =>
        pawns |= BitboardAttacks.attacksFrom(bitboards, square, Piece(color, PieceType.Pawn))
      }
      Bitboards.foreachSetBit(bitboards.knights(color)) { square =>
        knights |= BitboardAttacks.attacksFrom(bitboards, square, Piece(color, PieceType.Knight))
      }
      Bitboards.foreachSetBit(bitboards.bishops(color)) { square =>
        bishops |= BitboardAttacks.attacksFrom(bitboards, square, Piece(color, PieceType.Bishop))
      }
      Bitboards.foreachSetBit(bitboards.rooks(color)) { square =>
        rooks |= BitboardAttacks.attacksFrom(bitboards, square, Piece(color, PieceType.Rook))
      }
      Bitboards.foreachSetBit(bitboards.queens(color)) { square =>
        queens |= BitboardAttacks.attacksFrom(bitboards, square, Piece(color, PieceType.Queen))
      }
      Bitboards.foreachSetBit(bitboards.kings(color)) { square =>
        kings |= BitboardAttacks.attacksFrom(bitboards, square, Piece(color, PieceType.King))
      }

      AttackInfo(pawns, knights, bishops, rooks, queens, kings)

  private object EvalContext:
    def from(game: Game, profiler: Option[SearchProfiler] = None): EvalContext =
      val bitboards = game.board.bitboards
      val whiteAttacks = profileEval(profiler, _.recordEvalAttack) {
        AttackInfo.from(bitboards, Color.White)
      }
      val blackAttacks = profileEval(profiler, _.recordEvalAttack) {
        AttackInfo.from(bitboards, Color.Black)
      }
      val pieces = bitboards.pieceList(Color.White) ++ bitboards.pieceList(Color.Black)
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
        bitboards,
        whiteAttacks,
        blackAttacks,
        whitePieces,
        blackPieces,
        whitePawns,
        blackPawns,
        whiteKing,
        blackKing,
        whiteQueen,
        blackQueen,
        Math.min(phase, PestoTables.totalPhase),
        profiler
      )

  /**
   * Score all pieces for one color using PeSTO material + PST values,
   * interpolated between middlegame and endgame.
   */
  private def colorScore(pieces: List[(Pos, Piece)], isWhite: Boolean, phase: Int): Int =
    pieces.map { case (pos, piece) =>
      PestoTables.pieceScore(piece.kind, pos, isWhite, phase)
    }.sum

  private def staticEvaluate(game: Game, profiler: Option[SearchProfiler] = None): Int =
    val eval = profileEval(profiler, _.recordEvalContext) {
      EvalContext.from(game, profiler)
    }
    val phase = eval.phase
    val material = profileEval(profiler, _.recordEvalMaterial) {
      colorScore(eval.whitePieces, isWhite = true, phase) - colorScore(eval.blackPieces, isWhite = false, phase)
    }
    val pawns = profileEval(profiler, _.recordEvalPawn) {
      pawnStructureScore(eval, Color.White) - pawnStructureScore(eval, Color.Black)
    }
    val kings = profileEval(profiler, _.recordEvalKing) {
      kingSafetyScore(eval, Color.White) - kingSafetyScore(eval, Color.Black)
    }
    val activity = profileEval(profiler, _.recordEvalActivity) {
      pieceActivityScore(eval, Color.White) - pieceActivityScore(eval, Color.Black)
    }
    val queen = profileEval(profiler, _.recordEvalQueen) {
      queenInvasionScore(eval, Color.White) - queenInvasionScore(eval, Color.Black)
    }
    val mobility = profileEval(profiler, _.recordEvalMobility) {
      mobilityScore(eval, Color.White) - mobilityScore(eval, Color.Black)
    }
    val hanging = profileEval(profiler, _.recordEvalHanging) {
      hangingPiecesScore(eval, Color.White) - hangingPiecesScore(eval, Color.Black)
    }
    val diff =
      material + pawns + kings + activity + queen + mobility + hanging
    game.sideToMove match
      case Color.White => diff
      case Color.Black => -diff

  private def profileEval[A](profiler: Option[SearchProfiler], record: SearchProfiler => Long => Unit)(body: => A): A =
    profiler match
      case None => body
      case Some(p) =>
        val start = System.nanoTime()
        val result = body
        record(p)(System.nanoTime() - start)
        result

  private def pawnStructureScore(eval: EvalContext, color: Color): Int =
    val ownPawns = eval.bitboards.pawns(color)
    val enemyPawns = eval.bitboards.pawns(color.other)
    val ownFileCounts = Array.ofDim[Int](8)
    val enemyAdjacentForward = Array.fill[Long](8)(0L)

    Bitboards.foreachSetBit(ownPawns) { square =>
      ownFileCounts(square & 7) += 1
    }
    var file = 0
    while file < 8 do
      enemyAdjacentForward(file) =
        adjacentFilesInclusive(file).foldLeft(0L) { (acc, f) => acc | (enemyPawns & Bitboards.fileMask(f)) }
      file += 1

    var score = 0
    Bitboards.foreachSetBit(ownPawns) { square =>
      val pawn = Bitboards.pos(square)
      val doubledPenalty = if ownFileCounts(pawn.file) > 1 then -14 else 0
      val isolatedPenalty =
        if adjacentFiles(pawn.file).forall(file => ownFileCounts(file) == 0) then -18 else 0
      val backwardPenalty =
        if isBackwardPawn(eval, pawn, color, ownPawns) then -10 else 0
      val passedBonus =
        if isPassedPawn(pawn, color, enemyAdjacentForward(pawn.file)) then
          val advancement = if color == Color.White then pawn.rank else 7 - pawn.rank
          12 + advancement * advancement * 3
        else 0

      score += doubledPenalty + isolatedPenalty + backwardPenalty + passedBonus
    }
    score

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

        val shieldScore = pawnShieldScore(eval.bitboards, color, king)
        val openFilePenalty = kingFileExposurePenalty(eval, color, king)
        val dangerPenalty = kingAttackDangerPenalty(eval, color, king)
        castledBonus + shieldScore - openFilePenalty - dangerPenalty

  private def pawnShieldScore(bitboards: Bitboards, color: Color, king: Pos): Int =
    val dir = pawnDirection(color)
    val ownPawns = bitboards.pawns(color)
    adjacentFilesInclusive(king.file).map { file =>
      val front = Pos(file, king.rank + dir)
      val farFront = Pos(file, king.rank + 2 * dir)
      if front.inBounds && (ownPawns & Bitboards.mask(front)) != 0L then 12
      else if farFront.inBounds && (ownPawns & Bitboards.mask(farFront)) != 0L then 5
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
    val enemyAttacks = eval.attacksOf(color.other).all
    val attackedZoneSquares =
      zone.count(square => (enemyAttacks & Bitboards.mask(square)) != 0L)

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
        if (sameFile || sameRank || sameDiagonal) && clearPath(eval.bitboards, pos, king, eval.profiler) then
          if kind == PieceType.Queen then 35 else 22
        else 0
    }.sum

  private def isPassedPawn(pawn: Pos, color: Color, enemyAdjacentPawns: Long): Boolean =
    var passed = true
    Bitboards.foreachSetBit(enemyAdjacentPawns) { square =>
      if passed then
        val enemyRank = square >>> 3
        if isAhead(enemyRank, pawn.rank, color) then passed = false
    }
    passed

  private def isBackwardPawn(eval: EvalContext, pawn: Pos, color: Color, ownPawns: Long): Boolean =
    var supportedByNeighbor = false
    adjacentFiles(pawn.file).foreach { file =>
      Bitboards.foreachSetBit(ownPawns & Bitboards.fileMask(file)) { square =>
        if !supportedByNeighbor then
          val otherRank = square >>> 3
          if !isAhead(otherRank, pawn.rank, color) then supportedByNeighbor = true
      }
    }
    val front = Pos(pawn.file, pawn.rank + pawnDirection(color))
    val frontControlledByEnemy =
      front.inBounds && (eval.attacksOf(color.other).pawns & Bitboards.mask(front)) != 0L
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
    eval.piecesOf(color).map {
      case (pos, Piece(_, kind)) =>
        kind match
          case PieceType.Knight => knightMobility(eval.bitboards, color, pos) * 4
          case PieceType.Bishop => slidingMobility(eval.bitboards, color, pos, bishopDirections) * 3
          case PieceType.Rook   => slidingMobility(eval.bitboards, color, pos, rookDirections) * 2
          case PieceType.Queen  => if isOpeningPhase(eval.phase) then 0 else slidingMobility(eval.bitboards, color, pos, queenDirections)
          case PieceType.Pawn   => pawnMobility(eval.bitboards, color, pos) * 2
          case PieceType.King   => 0
    }.sum

  private def knightMobility(bitboards: Bitboards, color: Color, pos: Pos): Int =
    knightOffsets.count { offset =>
      val to = pos + offset
      to.inBounds && (bitboards.pieces(color) & Bitboards.mask(to)) == 0L
    }

  private def slidingMobility(bitboards: Bitboards, color: Color, pos: Pos, directions: List[(Int, Int)]): Int =
    directions.map(direction => rayMobility(bitboards, color, pos, direction)).sum

  private def rayMobility(bitboards: Bitboards, color: Color, pos: Pos, direction: (Int, Int)): Int =
    var current = pos + direction
    var count = 0
    var blocked = false
    val ownPieces = bitboards.pieces(color)
    val occupied = bitboards.occupied
    while current.inBounds && !blocked do
      val bit = Bitboards.mask(current)
      if (occupied & bit) == 0L then
        count += 1
        current = current + direction
      else
        if (ownPieces & bit) == 0L then count += 1
        blocked = true
    count

  private def pawnMobility(bitboards: Bitboards, color: Color, pos: Pos): Int =
    val dir = pawnDirection(color)
    val oneForward = pos + (0, dir)
    val quiet = if oneForward.inBounds && (bitboards.occupied & Bitboards.mask(oneForward)) == 0L then 1 else 0
    val enemies = bitboards.pieces(color.other)
    val captures =
      List(pos + (-1, dir), pos + (1, dir)).count { to =>
        to.inBounds && (enemies & Bitboards.mask(to)) != 0L
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
    val defenders = eval.piecesOf(color)
    val attackerInfo = eval.attacksOf(color.other)
    val defenderAttacks = eval.attacksOf(color).all
    defenders.collect {
      case (pos, piece) if piece.kind != PieceType.King =>
        leastAttackerValue(attackerInfo, pos) match
          case None => 0
          case Some(leastAttacker) =>
            val defended = (defenderAttacks & Bitboards.mask(pos)) != 0L
            val victim = pieceValue(piece.kind)
            val basePenalty =
              if !defended then victim / 3
              else if leastAttacker < victim then victim / 8
              else 0
            -Math.min(basePenalty, 300)
    }.sum

  private def leastAttackerValue(attackerInfo: AttackInfo, target: Pos): Option[Int] =
    val targetBit = Bitboards.mask(target)
    val attackerOrder = List(PieceType.King, PieceType.Pawn, PieceType.Knight, PieceType.Bishop, PieceType.Rook, PieceType.Queen)
    attackerOrder.find(kind => (attackerInfo.byKind(kind) & targetBit) != 0L).map(pieceValue)

  private def isAttackedBy(bitboards: Bitboards, attacker: Color, target: Pos, profiler: Option[SearchProfiler]): Boolean =
    profileEval(profiler, _.recordEvalAttack) {
      BitboardAttacks.isAttackedBy(bitboards, attacker, target)
    }

  private def isAttackedByKind(bitboards: Bitboards, attacker: Color, kind: PieceType, target: Pos, profiler: Option[SearchProfiler]): Boolean =
    profileEval(profiler, _.recordEvalAttack) {
      BitboardAttacks.isAttackedByKind(bitboards, attacker, kind, target)
    }

  private def attacksSquare(bitboards: Bitboards, from: Pos, piece: Piece, target: Pos, profiler: Option[SearchProfiler]): Boolean =
    profileEval(profiler, _.recordEvalAttack) {
      BitboardAttacks.attacksSquare(bitboards, from, piece, target)
    }

  private def clearPath(bitboards: Bitboards, from: Pos, to: Pos, profiler: Option[SearchProfiler]): Boolean =
    profileEval(profiler, _.recordEvalClearPath) {
      BitboardAttacks.clearPath(bitboards, from, to)
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
      val result = staticEvaluate(game, Some(context.profiler))
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
    if context.transpositionTable.size >= maxTranspositionEntries then trimTranspositionTable(context)
    val storedScore = scoreToTransposition(score, ply)
    context.transpositionTable.get(key) match
      case Some(existing) if existing.depth > depth =>
      case _ => context.transpositionTable.update(key, TranspositionEntry(depth, storedScore, bound, bestMove))

  private def trimTranspositionTable(context: SearchContext): Unit =
    val before = context.transpositionTable.size
    var removed = 0

    def removeUntilTarget(predicate: TranspositionEntry => Boolean): Unit =
      if context.transpositionTable.size > transpositionTrimTarget then
        val needed = context.transpositionTable.size - transpositionTrimTarget
        val keys =
          context.transpositionTable.iterator
            .collect { case (key, entry) if predicate(entry) => key }
            .take(needed)
            .toList
        keys.foreach { key =>
          if context.transpositionTable.remove(key).nonEmpty then removed += 1
        }

    removeUntilTarget(entry => entry.depth <= 1)
    removeUntilTarget(entry => entry.depth <= 2 && entry.bound != Bound.Exact)
    removeUntilTarget(entry => entry.depth <= 2)
    removeUntilTarget(entry => entry.depth <= 3 && entry.bound != Bound.Exact)
    removeUntilTarget(_ => true)

    if removed > 0 then
      println(s"  tt trim | $before -> ${context.transpositionTable.size} entries")

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
