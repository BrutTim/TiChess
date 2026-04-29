package ch.tichess.bot

import ch.tichess.controller.AppState
import ch.tichess.model.{Color, Game, Move, PieceType}

import scala.concurrent.Future
import ch.tichess.model.Fen

/**
 * Simple alpha-beta bot with a lightweight material-based heuristic.
 *
 * Note: this is intended as an MVP and therefore keeps the evaluation cheap.
 */
class AlphaBetaBot(thinkTimeMs: Long = 5000L, openingDb: Option[OpeningDatabase] = None) extends ChessBot:
  override val name: String = s"AlphaBetaBot(time=${thinkTimeMs}ms)"

  private class TimeLimitExceededException extends RuntimeException

  override def chooseMove(state: AppState): Future[Either[String, Move]] =
    val game = state.game
    val legal = game.legalMoves
    if legal.isEmpty then Future.successful(Left("No legal moves available."))
    else
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
        else
          // Execute CPU-heavy search in a global thread pool
          Future {
            val deadline = System.currentTimeMillis() + thinkTimeMs
            var bestMoveSoFar = legal.head
            var bestScoreSoFar = -mateScore
            var currentDepth = 1

            try
              // Iterative Deepening
              while System.currentTimeMillis() < deadline do
                val (mv, score) = searchBestMove(game, legal, currentDepth, deadline)
                bestMoveSoFar = mv
                bestScoreSoFar = score
                // If we found a mate, no need to search deeper
                if score >= mateScore - 100 then throw new TimeLimitExceededException()
                currentDepth += 1
            catch
              case _: TimeLimitExceededException => // Search aborted, keep best move from last completed depth

            Right(bestMoveSoFar)
          }
      }

  private def searchBestMove(game: Game, legal: List[Move], depth: Int, deadline: Long): (Move, Int) =
    var bestMove = legal.head
    var bestScore = -mateScore * 2

    var alpha = -mateScore * 2
    val beta = mateScore * 2

    val ordered = orderMoves(game, legal)
    var i = 0
    while i < ordered.size && alpha < beta do
      if System.currentTimeMillis() > deadline then throw new TimeLimitExceededException()

      val mv = ordered(i)
      val next = game.applyMove(mv).toOption.get
      val score = -negamax(next, depth - 1, -beta, -alpha, deadline)

      if score > bestScore then
        bestScore = score
        bestMove = mv

      alpha = Math.max(alpha, bestScore)
      i += 1

    (bestMove, bestScore)

  private def negamax(game: Game, depth: Int, alpha0: Int, beta0: Int, deadline: Long): Int =
    if System.currentTimeMillis() > deadline then throw new TimeLimitExceededException()

    if depth <= 0 || game.isCheckmate || game.isDraw then
      evaluate(game)
    else
      var alpha = alpha0
      var best = -mateScore * 2

      val legal = game.legalMoves
      val ordered = orderMoves(game, legal)

      var i = 0
      while i < ordered.size && alpha < beta0 do
        val mv = ordered(i)
        val next = game.applyMove(mv).toOption.get
        val score = -negamax(next, depth - 1, -beta0, -alpha, deadline)
        best = Math.max(best, score)
        alpha = Math.max(alpha, best)
        i += 1

      best

  private val mateScore: Int = 100000

  private def evaluate(game: Game): Int =
    if game.isCheckmate then
      // "game.sideToMove" is checkmated -> bad for the current player.
      -mateScore
    else if game.isDraw then 0
    else
      // Material from side-to-move perspective.
      val diff = materialBalance(game) // positive => White leads
      val sidePerspective = game.sideToMove match
        case Color.White => diff
        case Color.Black => -diff

      // Mobility bonus for the current player.
      val mobility = game.legalMoves.size

      sidePerspective * 10 + mobility

  private def materialBalance(game: Game): Int =
    val (white, black) = game.board.allPieces.values.partition(_.color == Color.White)
    materialScore(white) - materialScore(black)

  private def materialScore(pieces: Iterable[_]): Int =
    pieces
      .map {
        case ch.tichess.model.Piece(_, kind) => pieceValue(kind)
        case _                               => 0
      }
      .sum

  private def pieceValue(kind: PieceType): Int = kind match
    case PieceType.Pawn => 1
    case PieceType.Knight => 3
    case PieceType.Bishop => 3
    case PieceType.Rook => 5
    case PieceType.Queen => 9
    case PieceType.King => 0

  private def orderMoves(game: Game, moves: List[Move]): List[Move] =
    // Captures and promotions first: a cheap ordering helps alpha-beta prune.
    moves.sortBy(mv => -captureUrgency(game, mv))

  private def captureUrgency(game: Game, mv: Move): Int =
    game.board.pieceAt(mv.from).map { moved =>
      val fromKind = moved.kind
      val target = game.board.pieceAt(mv.to)

      val captureValue =
        target match
          case Some(t) => pieceValue(t.kind)
          case None =>
            // En-passant capture: destination square is empty but enPassantTarget matches.
            if fromKind == PieceType.Pawn && game.enPassantTarget.contains(mv.to) then pieceValue(PieceType.Pawn)
            else 0

      val promotionValue =
        mv.promotion match
          case Some(role) =>
            // Promotion is effectively gaining the promoted piece value.
            val promotedKind = role.toPieceType
            pieceValue(promotedKind) - pieceValue(PieceType.Pawn)
          case None => 0

      captureValue + promotionValue
    }.getOrElse(0)

