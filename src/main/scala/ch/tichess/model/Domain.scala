package ch.tichess.model

enum Color:
  case White, Black
  def other: Color = this match
    case White => Black
    case Black => White

enum PieceType:
  case King, Queen, Rook, Bishop, Knight, Pawn

final case class Piece(color: Color, kind: PieceType)

final case class Pos(file: Int, rank: Int):
  def inBounds: Boolean = file >= 0 && file < 8 && rank >= 0 && rank < 8
  def +(d: (Int, Int)): Pos = Pos(file + d._1, rank + d._2)

object Pos:
  def fromAlgebraic(s: String): Either[String, Pos] =
    if s.length != 2 then Left("Position must have length 2 (e.g. e2).")
    else
      val f = s.charAt(0)
      val r = s.charAt(1)
      val file = f - 'a'
      val rank = r - '1'
      val p = Pos(file, rank)
      if p.inBounds then Right(p) else Left("Position out of bounds.")

final case class Move(from: Pos, to: Pos)
