package ch.tichess.model

final class Board private (val pieces: Map[Pos, Piece], val bitboards: Bitboards):
  def pieceAt(pos: Pos): Option[Piece] = bitboards.pieceAt(pos)
  def isEmpty(pos: Pos): Boolean = (bitboards.occupied & Bitboards.mask(pos)) == 0L
  def allPieces: Map[Pos, Piece] = pieces

  def copy(pieces: Map[Pos, Piece] = this.pieces): Board = Board(pieces)

  def removeAt(pos: Pos): Board =
    pieces.get(pos) match
      case None => this
      case Some(piece) => Board.unsafe(pieces - pos, bitboards.remove(pos, piece))

  def setAt(pos: Pos, piece: Piece): Board =
    val withoutExisting =
      pieces.get(pos) match
        case Some(existing) => bitboards.remove(pos, existing)
        case None           => bitboards
    Board.unsafe(pieces.updated(pos, piece), withoutExisting.add(pos, piece))

  def movePiece(move: Move): Either[String, Board] =
    pieces.get(move.from) match
      case None => Left("No piece at source position.")
      case Some(p) =>
        val updated = pieces - move.from - move.to + (move.to -> p)
        val captured = pieces.get(move.to)
        Right(Board.unsafe(updated, bitboards.move(move.from, move.to, p, captured)))

  override def equals(other: Any): Boolean =
    other match
      case board: Board => pieces == board.pieces
      case _            => false

  override def hashCode(): Int = pieces.hashCode()

  override def toString: String = s"Board($pieces)"

object Board:
  def apply(pieces: Map[Pos, Piece]): Board =
    Board(pieces, Bitboards.fromPieces(pieces))

  private[model] def unsafe(pieces: Map[Pos, Piece], bitboards: Bitboards): Board =
    new Board(pieces, bitboards)

  private def apply(pieces: Map[Pos, Piece], bitboards: Bitboards): Board =
    new Board(pieces, bitboards)

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
