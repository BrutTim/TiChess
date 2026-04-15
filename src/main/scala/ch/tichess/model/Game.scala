package ch.tichess.model

final case class Game(
    board: Board,
    sideToMove: Color,
    castlingRights: CastlingRights = CastlingRights.initial,
    enPassantTarget: Option[Pos] = None,
    halfMoveClock: Int = 0,
    fullMoveNumber: Int = 1
):
  def isInCheck: Boolean = Rules.isInCheck(board, sideToMove)
  def isDraw: Boolean = Rules.isDraw(this)

  def legalMoves: List[Move] =
    val ownPositions =
      board.allPieces.collect { case (pos, piece) if piece.color == sideToMove => pos }.toList
    val allTargets =
      (0 until 8).flatMap(file => (0 until 8).map(rank => Pos(file, rank))).toList

    ownPositions.flatMap { from =>
      allTargets.flatMap { to =>
        candidateMoves(from, to).flatMap { move =>
          Rules.validateMove(this, move).toOption.flatMap { _ =>
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
      _ <- Rules.validateMove(this, move)
      _ <- requirePromotionChoice(move)
      nextBoard <- applyMoveToBoard(move)
      _ <-
        if Rules.isInCheck(nextBoard, sideToMove) then Left("Illegal move: king would remain in check.")
        else Right(())
    yield
      val piece = board.pieceAt(move.from).get
      val isPawnMove = piece.kind == PieceType.Pawn
      val isCapture = board.pieceAt(move.to).isDefined || (isPawnMove && enPassantTarget.contains(move.to))
      val newHalfMove = if isPawnMove || isCapture then 0 else halfMoveClock + 1
      val newFullMove = if sideToMove == Color.Black then fullMoveNumber + 1 else fullMoveNumber
      
      val newEnPassant = if isPawnMove && Math.abs(move.to.rank - move.from.rank) == 2 then
        Some(Pos(move.from.file, (move.from.rank + move.to.rank) / 2))
      else None

      var nextCastling = castlingRights
      if piece.kind == PieceType.King then
        nextCastling = if sideToMove == Color.White then nextCastling.revokeWhite else nextCastling.revokeBlack
      else if piece.kind == PieceType.Rook then
        if move.from == Pos(0, 0) then nextCastling = nextCastling.copy(whiteQueenside = false)
        else if move.from == Pos(7, 0) then nextCastling = nextCastling.copy(whiteKingside = false)
        else if move.from == Pos(0, 7) then nextCastling = nextCastling.copy(blackQueenside = false)
        else if move.from == Pos(7, 7) then nextCastling = nextCastling.copy(blackKingside = false)
        
      if move.to == Pos(0, 0) then nextCastling = nextCastling.copy(whiteQueenside = false)
      else if move.to == Pos(7, 0) then nextCastling = nextCastling.copy(whiteKingside = false)
      else if move.to == Pos(0, 7) then nextCastling = nextCastling.copy(blackQueenside = false)
      else if move.to == Pos(7, 7) then nextCastling = nextCastling.copy(blackKingside = false)

      Game(nextBoard, sideToMove.other, nextCastling, newEnPassant, newHalfMove, newFullMove)

  private def candidateMoves(from: Pos, to: Pos): List[Move] =
    board.pieceAt(from) match
      case Some(Piece(color, PieceType.Pawn)) if promotionRank(color, to.rank) =>
        PromotionRole.values.toList.map(role => Move(from, to, Some(role)))
      case _ =>
        List(Move(from, to))

  private def applyMoveToBoard(move: Move): Either[String, Board] =
    val p = board.pieceAt(move.from).get
    for
      baseBoard <- board.movePiece(move)
    yield
      val epBoard = if p.kind == PieceType.Pawn && enPassantTarget.contains(move.to) then
        baseBoard.copy(pieces = baseBoard.pieces - Pos(move.to.file, move.from.rank))
      else baseBoard

      val castlingBoard = if p.kind == PieceType.King && Math.abs(move.to.file - move.from.file) == 2 then
        if move.to.file == 6 then
          epBoard.movePiece(Move(Pos(7, move.to.rank), Pos(5, move.to.rank))).getOrElse(epBoard)
        else
          epBoard.movePiece(Move(Pos(0, move.to.rank), Pos(3, move.to.rank))).getOrElse(epBoard)
      else epBoard

      promoteIfNeeded(castlingBoard, move)

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
