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
    val candidates =
      board.allPieces.toList.flatMap {
        case (from, piece) if piece.color == sideToMove => pseudoMoves(from, piece)
        case _ => Nil
      }

    candidates.flatMap { move =>
      Rules.validateMove(this, move).toOption.flatMap { _ =>
        applyMoveToBoard(move).toOption.flatMap { nextBoard =>
          if Rules.isInCheck(nextBoard, sideToMove) then None else Some(move)
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

  private def pseudoMoves(from: Pos, piece: Piece): List[Move] =
    piece.kind match
      case PieceType.Pawn   => pseudoPawnMoves(from, piece.color)
      case PieceType.Knight => pseudoLeaperMoves(from, knightOffsets)
      case PieceType.Bishop => pseudoSlidingMoves(from, bishopDirections)
      case PieceType.Rook   => pseudoSlidingMoves(from, rookDirections)
      case PieceType.Queen  => pseudoSlidingMoves(from, queenDirections)
      case PieceType.King   => pseudoKingMoves(from)

  private def pseudoPawnMoves(from: Pos, color: Color): List[Move] =
    val dir = if color == Color.White then 1 else -1
    val startRank = if color == Color.White then 1 else 6
    val oneForward = from + (0, dir)
    val twoForward = from + (0, 2 * dir)
    val captures = List(from + (-1, dir), from + (1, dir))

    val quietMoves =
      if oneForward.inBounds && board.isEmpty(oneForward) then
        val single = movesWithPromotion(from, oneForward, color)
        if from.rank == startRank && twoForward.inBounds && board.isEmpty(twoForward) then
          single :+ Move(from, twoForward)
        else single
      else Nil

    val captureMoves =
      captures.flatMap { to =>
        val capturesEnemy = to.inBounds && board.pieceAt(to).exists(_.color != color)
        val capturesEnPassant = enPassantTarget.contains(to)
        if capturesEnemy || capturesEnPassant then movesWithPromotion(from, to, color)
        else Nil
      }

    quietMoves ++ captureMoves

  private def movesWithPromotion(from: Pos, to: Pos, color: Color): List[Move] =
    if promotionRank(color, to.rank) then
      PromotionRole.values.toList.map(role => Move(from, to, Some(role)))
    else List(Move(from, to))

  private def pseudoLeaperMoves(from: Pos, offsets: List[(Int, Int)]): List[Move] =
    offsets.flatMap { offset =>
      val to = from + offset
      if canMoveTo(to) then Some(Move(from, to)) else None
    }

  private def pseudoSlidingMoves(from: Pos, directions: List[(Int, Int)]): List[Move] =
    directions.flatMap(direction => rayMoves(from, direction))

  private def rayMoves(from: Pos, direction: (Int, Int)): List[Move] =
    val moves = scala.collection.mutable.ListBuffer.empty[Move]
    var current = from + direction
    var blocked = false
    while current.inBounds && !blocked do
      board.pieceAt(current) match
        case None =>
          moves += Move(from, current)
          current = current + direction
        case Some(piece) =>
          if piece.color != sideToMove then moves += Move(from, current)
          blocked = true
    moves.toList

  private def pseudoKingMoves(from: Pos): List[Move] =
    val normalMoves = pseudoLeaperMoves(from, kingOffsets)
    val castlingMoves = List(Move(from, from + (2, 0)), Move(from, from + (-2, 0))).filter(move => move.to.inBounds)
    normalMoves ++ castlingMoves

  private def canMoveTo(to: Pos): Boolean =
    to.inBounds && !board.pieceAt(to).exists(_.color == sideToMove)

  private val knightOffsets: List[(Int, Int)] =
    List((1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2))

  private val kingOffsets: List[(Int, Int)] =
    List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1))

  private val bishopDirections: List[(Int, Int)] =
    List((1, 1), (1, -1), (-1, 1), (-1, -1))

  private val rookDirections: List[(Int, Int)] =
    List((1, 0), (-1, 0), (0, 1), (0, -1))

  private val queenDirections: List[(Int, Int)] =
    bishopDirections ++ rookDirections

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
