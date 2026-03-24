package ch.tichess.model

final case class Board(pieces: Map[Pos, Piece]):
  def pieceAt(pos: Pos): Option[Piece] = pieces.get(pos)
  def isEmpty(pos: Pos): Boolean = !pieces.contains(pos)
  def allPieces: Map[Pos, Piece] = pieces

  def removeAt(pos: Pos): Board = Board(pieces - pos)

  def movePiece(move: Move): Either[String, Board] =
    pieces.get(move.from) match
      case None => Left("No piece at source position.")
      case Some(p) =>
        val updated = pieces - move.from - move.to + (move.to -> p)
        Right(Board(updated))

object Board:
  def empty: Board = Board(Map.empty)

  def initial: Board =
    val backRank: List[PieceType] =
      List(
        PieceType.Rook,
        PieceType.Knight,
        PieceType.Bishop,
        PieceType.Queen,
        PieceType.King,
        PieceType.Bishop,
        PieceType.Knight,
        PieceType.Rook
      )

    def placeBack(rank: Int, color: Color): Map[Pos, Piece] =
      backRank.zipWithIndex.map { (k, file) => (Pos(file, rank), Piece(color, k)) }.toMap

    def placePawns(rank: Int, color: Color): Map[Pos, Piece] =
      (0 until 8).map(file => (Pos(file, rank), Piece(color, PieceType.Pawn))).toMap

    val white = placeBack(0, Color.White) ++ placePawns(1, Color.White)
    val black = placeBack(7, Color.Black) ++ placePawns(6, Color.Black)
    Board(white ++ black)
