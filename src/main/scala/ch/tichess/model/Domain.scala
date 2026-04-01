package ch.tichess.model

enum Color:
  case White, Black
  def other: Color = this match
    case White => Black
    case Black => White

enum PieceType:
  case King, Queen, Rook, Bishop, Knight, Pawn

enum PromotionRole:
  case Queen, Rook, Bishop, Knight

  def toPieceType: PieceType = this match
    case PromotionRole.Queen  => PieceType.Queen
    case PromotionRole.Rook   => PieceType.Rook
    case PromotionRole.Bishop => PieceType.Bishop
    case PromotionRole.Knight => PieceType.Knight

object PieceType:
  val promotableRoles: Set[PieceType] =
    PromotionRole.values.map(_.toPieceType).toSet

object PromotionRole:
  def fromPromotionChar(s: String): Either[String, PromotionRole] =
    s.toLowerCase match
      case "q" => Right(PromotionRole.Queen)
      case "r" => Right(PromotionRole.Rook)
      case "b" => Right(PromotionRole.Bishop)
      case "n" => Right(PromotionRole.Knight)
      case _   => Left("Promotion must be one of: q, r, b, n.")

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

final case class Move(from: Pos, to: Pos, promotion: Option[PromotionRole] = None)
