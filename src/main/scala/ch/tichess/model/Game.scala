package ch.tichess.model

final case class Game(board: Board, sideToMove: Color):
  def isInCheck: Boolean = Rules.isInCheck(board, sideToMove)

  def legalMoves: List[Move] =
    val ownPositions =
      board.allPieces.collect { case (pos, piece) if piece.color == sideToMove => pos }.toList
    val allTargets =
      (0 until 8).flatMap(file => (0 until 8).map(rank => Pos(file, rank))).toList

    ownPositions.flatMap { from =>
      allTargets.flatMap { to =>
        candidateMoves(from, to).flatMap { move =>
          Rules.validateMove(board, sideToMove, move).toOption.flatMap { _ =>
            applyMoveToBoard(move).toOption.flatMap { nextBoard =>
              if Rules.isInCheck(nextBoard, sideToMove) then None else Some(move)
            }
          }
        }
      }
    }

  def isCheckmate: Boolean = isInCheck && legalMoves.isEmpty

  def applyMove(move: Move): Either[String, Game] =
    for
      _ <- Rules.validateMove(board, sideToMove, move)
      _ <- requirePromotionChoice(move)
      nextBoard <- applyMoveToBoard(move)
      _ <-
        if Rules.isInCheck(nextBoard, sideToMove) then Left("Illegal move: king would remain in check.")
        else Right(())
    yield Game(nextBoard, sideToMove.other)

  private def candidateMoves(from: Pos, to: Pos): List[Move] =
    board.pieceAt(from) match
      case Some(Piece(color, PieceType.Pawn)) if promotionRank(color, to.rank) =>
        PromotionRole.values.toList.map(role => Move(from, to, Some(role)))
      case _ =>
        List(Move(from, to))

  private def applyMoveToBoard(move: Move): Either[String, Board] =
    for
      movedBoard <- board.movePiece(move)
    yield promoteIfNeeded(movedBoard, move)

  private def requirePromotionChoice(move: Move): Either[String, Unit] =
    board.pieceAt(move.from) match
      case Some(Piece(color, PieceType.Pawn)) if promotionRank(color, move.to.rank) && move.promotion.isEmpty =>
        Left("Promotion required: choose q, r, b, or n.")
      case _ =>
        Right(())

  private def promoteIfNeeded(nextBoard: Board, move: Move): Board =
    nextBoard.pieceAt(move.to) match
      case Some(Piece(color, PieceType.Pawn)) if promotionRank(color, move.to.rank) =>
        val promotedKind = move.promotion.map(_.toPieceType).getOrElse(PieceType.Pawn)
        nextBoard.copy(pieces = nextBoard.pieces.updated(move.to, Piece(color, promotedKind)))
      case _ =>
        nextBoard

  private def promotionRank(color: Color, rank: Int): Boolean =
    (color == Color.White && rank == 7) || (color == Color.Black && rank == 0)

object Game:
  def initial: Game = Game(Board.initial, Color.White)
