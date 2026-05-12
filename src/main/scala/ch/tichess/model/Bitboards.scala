package ch.tichess.model

final case class Bitboards(
    whitePawns: Long,
    whiteKnights: Long,
    whiteBishops: Long,
    whiteRooks: Long,
    whiteQueens: Long,
    whiteKing: Long,
    blackPawns: Long,
    blackKnights: Long,
    blackBishops: Long,
    blackRooks: Long,
    blackQueens: Long,
    blackKing: Long
):
  def whitePieces: Long =
    whitePawns | whiteKnights | whiteBishops | whiteRooks | whiteQueens | whiteKing

  def blackPieces: Long =
    blackPawns | blackKnights | blackBishops | blackRooks | blackQueens | blackKing

  def occupied: Long = whitePieces | blackPieces
  def empty: Long = ~occupied

  def pieces(color: Color): Long =
    color match
      case Color.White => whitePieces
      case Color.Black => blackPieces

  def pawns(color: Color): Long =
    color match
      case Color.White => whitePawns
      case Color.Black => blackPawns

  def knights(color: Color): Long =
    color match
      case Color.White => whiteKnights
      case Color.Black => blackKnights

  def bishops(color: Color): Long =
    color match
      case Color.White => whiteBishops
      case Color.Black => blackBishops

  def rooks(color: Color): Long =
    color match
      case Color.White => whiteRooks
      case Color.Black => blackRooks

  def queens(color: Color): Long =
    color match
      case Color.White => whiteQueens
      case Color.Black => blackQueens

  def kings(color: Color): Long =
    color match
      case Color.White => whiteKing
      case Color.Black => blackKing

  def byPiece(color: Color, kind: PieceType): Long =
    kind match
      case PieceType.Pawn   => pawns(color)
      case PieceType.Knight => knights(color)
      case PieceType.Bishop => bishops(color)
      case PieceType.Rook   => rooks(color)
      case PieceType.Queen  => queens(color)
      case PieceType.King   => kings(color)

  def pieceAt(pos: Pos): Option[Piece] =
    val bit = Bitboards.mask(pos)
    if (whitePawns & bit) != 0L then Some(Piece(Color.White, PieceType.Pawn))
    else if (whiteKnights & bit) != 0L then Some(Piece(Color.White, PieceType.Knight))
    else if (whiteBishops & bit) != 0L then Some(Piece(Color.White, PieceType.Bishop))
    else if (whiteRooks & bit) != 0L then Some(Piece(Color.White, PieceType.Rook))
    else if (whiteQueens & bit) != 0L then Some(Piece(Color.White, PieceType.Queen))
    else if (whiteKing & bit) != 0L then Some(Piece(Color.White, PieceType.King))
    else if (blackPawns & bit) != 0L then Some(Piece(Color.Black, PieceType.Pawn))
    else if (blackKnights & bit) != 0L then Some(Piece(Color.Black, PieceType.Knight))
    else if (blackBishops & bit) != 0L then Some(Piece(Color.Black, PieceType.Bishop))
    else if (blackRooks & bit) != 0L then Some(Piece(Color.Black, PieceType.Rook))
    else if (blackQueens & bit) != 0L then Some(Piece(Color.Black, PieceType.Queen))
    else if (blackKing & bit) != 0L then Some(Piece(Color.Black, PieceType.King))
    else None

  def kingSquare(color: Color): Option[Pos] =
    val king = kings(color)
    if king == 0L then None else Some(Bitboards.pos(java.lang.Long.numberOfTrailingZeros(king)))

  def pieceList(color: Color): List[(Pos, Piece)] =
    val out = scala.collection.mutable.ListBuffer.empty[(Pos, Piece)]
    PieceType.values.foreach { kind =>
      Bitboards.foreachSetBit(byPiece(color, kind)) { square =>
        out += Bitboards.pos(square) -> Piece(color, kind)
      }
    }
    out.toList

  def add(pos: Pos, piece: Piece): Bitboards =
    Bitboards.updated(this, pos, piece, add = true)

  def remove(pos: Pos, piece: Piece): Bitboards =
    Bitboards.updated(this, pos, piece, add = false)

  def move(from: Pos, to: Pos, piece: Piece, captured: Option[Piece]): Bitboards =
    val withoutMovingPiece = remove(from, piece)
    val withoutCaptured = captured match
      case Some(target) => withoutMovingPiece.remove(to, target)
      case None         => withoutMovingPiece
    withoutCaptured.add(to, piece)

object Bitboards:
  val empty: Bitboards = Bitboards(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)

  def index(pos: Pos): Int = pos.rank * 8 + pos.file
  def pos(index: Int): Pos = Pos(index & 7, index >>> 3)
  def mask(pos: Pos): Long = 1L << index(pos)
  def mask(index: Int): Long = 1L << index
  def popCount(bits: Long): Int = java.lang.Long.bitCount(bits)
  def knightAttacks(index: Int): Long = knightAttackMasks(index)
  def kingAttacks(index: Int): Long = kingAttackMasks(index)
  def fileMask(file: Int): Long = fileMasks(file)

  def foreachSetBit(bits: Long)(f: Int => Unit): Unit =
    var remaining = bits
    while remaining != 0L do
      val square = java.lang.Long.numberOfTrailingZeros(remaining)
      f(square)
      remaining &= remaining - 1

  def fromBoard(board: Board): Bitboards =
    fromPieces(board.allPieces)

  def fromPieces(pieces: Map[Pos, Piece]): Bitboards =
    var whitePawns = 0L
    var whiteKnights = 0L
    var whiteBishops = 0L
    var whiteRooks = 0L
    var whiteQueens = 0L
    var whiteKing = 0L
    var blackPawns = 0L
    var blackKnights = 0L
    var blackBishops = 0L
    var blackRooks = 0L
    var blackQueens = 0L
    var blackKing = 0L

    pieces.foreach { case (pos, piece) =>
      val bit = mask(pos)
      (piece.color, piece.kind) match
        case (Color.White, PieceType.Pawn)   => whitePawns |= bit
        case (Color.White, PieceType.Knight) => whiteKnights |= bit
        case (Color.White, PieceType.Bishop) => whiteBishops |= bit
        case (Color.White, PieceType.Rook)   => whiteRooks |= bit
        case (Color.White, PieceType.Queen)  => whiteQueens |= bit
        case (Color.White, PieceType.King)   => whiteKing |= bit
        case (Color.Black, PieceType.Pawn)   => blackPawns |= bit
        case (Color.Black, PieceType.Knight) => blackKnights |= bit
        case (Color.Black, PieceType.Bishop) => blackBishops |= bit
        case (Color.Black, PieceType.Rook)   => blackRooks |= bit
        case (Color.Black, PieceType.Queen)  => blackQueens |= bit
        case (Color.Black, PieceType.King)   => blackKing |= bit
    }

    Bitboards(
      whitePawns,
      whiteKnights,
      whiteBishops,
      whiteRooks,
      whiteQueens,
      whiteKing,
      blackPawns,
      blackKnights,
      blackBishops,
      blackRooks,
      blackQueens,
      blackKing
    )

  private def updated(bitboards: Bitboards, pos: Pos, piece: Piece, add: Boolean): Bitboards =
    val bit = mask(pos)
    val nextMask =
      if add then (current: Long) => current | bit
      else (current: Long) => current & ~bit

    (piece.color, piece.kind) match
      case (Color.White, PieceType.Pawn)   => bitboards.copy(whitePawns = nextMask(bitboards.whitePawns))
      case (Color.White, PieceType.Knight) => bitboards.copy(whiteKnights = nextMask(bitboards.whiteKnights))
      case (Color.White, PieceType.Bishop) => bitboards.copy(whiteBishops = nextMask(bitboards.whiteBishops))
      case (Color.White, PieceType.Rook)   => bitboards.copy(whiteRooks = nextMask(bitboards.whiteRooks))
      case (Color.White, PieceType.Queen)  => bitboards.copy(whiteQueens = nextMask(bitboards.whiteQueens))
      case (Color.White, PieceType.King)   => bitboards.copy(whiteKing = nextMask(bitboards.whiteKing))
      case (Color.Black, PieceType.Pawn)   => bitboards.copy(blackPawns = nextMask(bitboards.blackPawns))
      case (Color.Black, PieceType.Knight) => bitboards.copy(blackKnights = nextMask(bitboards.blackKnights))
      case (Color.Black, PieceType.Bishop) => bitboards.copy(blackBishops = nextMask(bitboards.blackBishops))
      case (Color.Black, PieceType.Rook)   => bitboards.copy(blackRooks = nextMask(bitboards.blackRooks))
      case (Color.Black, PieceType.Queen)  => bitboards.copy(blackQueens = nextMask(bitboards.blackQueens))
      case (Color.Black, PieceType.King)   => bitboards.copy(blackKing = nextMask(bitboards.blackKing))

  private val knightAttackMasks: Array[Long] =
    Array.tabulate(64) { index =>
      attackMask(index, List((1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1), (-2, 1), (-1, 2)))
    }

  private val kingAttackMasks: Array[Long] =
    Array.tabulate(64) { index =>
      attackMask(index, List((1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)))
    }

  private val fileMasks: Array[Long] =
    Array.tabulate(8) { file =>
      var bits = 0L
      var rank = 0
      while rank < 8 do
        bits |= mask(Pos(file, rank))
        rank += 1
      bits
    }

  private def attackMask(index: Int, offsets: List[(Int, Int)]): Long =
    val file = index & 7
    val rank = index >>> 3
    var bits = 0L
    offsets.foreach { case (df, dr) =>
      val pos = Pos(file + df, rank + dr)
      if pos.inBounds then bits |= mask(pos)
    }
    bits

private[ch] object BitboardAttacks:
  def isInCheck(bitboards: Bitboards, color: Color): Boolean =
    bitboards.kingSquare(color).exists { king =>
      isAttackedBy(bitboards, color.other, king)
    }

  def isAttackedBy(bitboards: Bitboards, attacker: Color, target: Pos): Boolean =
    val targetIndex = Bitboards.index(target)
    PieceType.values.exists(kind => isAttackedByKind(bitboards, attacker, kind, targetIndex))

  def isAttackedByKind(bitboards: Bitboards, attacker: Color, kind: PieceType, target: Pos): Boolean =
    isAttackedByKind(bitboards, attacker, kind, Bitboards.index(target))

  def attacksSquare(bitboards: Bitboards, from: Pos, piece: Piece, target: Pos): Boolean =
    if from == target then false
    else attacksSquare(bitboards, Bitboards.index(from), piece, Bitboards.index(target))

  def attacksFrom(bitboards: Bitboards, fromIndex: Int, piece: Piece): Long =
    piece.kind match
      case PieceType.King =>
        Bitboards.kingAttacks(fromIndex)
      case PieceType.Knight =>
        Bitboards.knightAttacks(fromIndex)
      case PieceType.Pawn =>
        pawnAttacksFrom(fromIndex, piece.color)
      case PieceType.Bishop =>
        slidingAttacksFrom(bitboards.occupied, fromIndex, diagonalDirections)
      case PieceType.Rook =>
        slidingAttacksFrom(bitboards.occupied, fromIndex, orthogonalDirections)
      case PieceType.Queen =>
        slidingAttacksFrom(bitboards.occupied, fromIndex, diagonalDirections) |
          slidingAttacksFrom(bitboards.occupied, fromIndex, orthogonalDirections)

  def attacksSquare(bitboards: Bitboards, fromIndex: Int, piece: Piece, targetIndex: Int): Boolean =
    if fromIndex == targetIndex then false
    else
      val fromFile = fromIndex & 7
      val fromRank = fromIndex >>> 3
      val targetFile = targetIndex & 7
      val targetRank = targetIndex >>> 3
      val df = targetFile - fromFile
      val dr = targetRank - fromRank
      val absDf = Math.abs(df)
      val absDr = Math.abs(dr)

      piece.kind match
        case PieceType.King =>
          absDf <= 1 && absDr <= 1
        case PieceType.Queen =>
          isQueenLine(df, dr, absDf) && clearPath(bitboards.occupied, fromIndex, targetIndex)
        case PieceType.Rook =>
          isRookLine(df, dr) && clearPath(bitboards.occupied, fromIndex, targetIndex)
        case PieceType.Bishop =>
          isBishopLine(df, absDf, absDr) && clearPath(bitboards.occupied, fromIndex, targetIndex)
        case PieceType.Knight =>
          (absDf == 2 && absDr == 1) || (absDf == 1 && absDr == 2)
        case PieceType.Pawn =>
          val dir = if piece.color == Color.White then 1 else -1
          absDf == 1 && dr == dir

  def clearPath(bitboards: Bitboards, from: Pos, to: Pos): Boolean =
    clearPath(bitboards.occupied, Bitboards.index(from), Bitboards.index(to))

  def clearPath(occupied: Long, fromIndex: Int, targetIndex: Int): Boolean =
    val fromFile = fromIndex & 7
    val fromRank = fromIndex >>> 3
    val targetFile = targetIndex & 7
    val targetRank = targetIndex >>> 3
    val stepFile = Integer.signum(targetFile - fromFile)
    val stepRank = Integer.signum(targetRank - fromRank)
    val steps = Math.max(Math.abs(targetFile - fromFile), Math.abs(targetRank - fromRank)) - 1
    val step = stepFile + stepRank * 8

    var i = 1
    var square = fromIndex + step
    while i <= steps do
      if (occupied & Bitboards.mask(square)) != 0L then return false
      square += step
      i += 1
    true

  private def sliderAttacksTarget(occupied: Long, sliders: Long, targetIndex: Int, diagonal: Boolean): Boolean =
    var remaining = sliders
    while remaining != 0L do
      val fromIndex = java.lang.Long.numberOfTrailingZeros(remaining)
      val fromFile = fromIndex & 7
      val fromRank = fromIndex >>> 3
      val targetFile = targetIndex & 7
      val targetRank = targetIndex >>> 3
      val df = targetFile - fromFile
      val dr = targetRank - fromRank
      val onLine =
        if diagonal then Math.abs(df) == Math.abs(dr) && df != 0
        else (df == 0 && dr != 0) || (dr == 0 && df != 0)
      if onLine && clearPath(occupied, fromIndex, targetIndex) then return true
      remaining &= remaining - 1
    false

  private def isAttackedByKind(bitboards: Bitboards, attacker: Color, kind: PieceType, targetIndex: Int): Boolean =
    kind match
      case PieceType.Pawn =>
        (bitboards.pawns(attacker) & pawnAttackersTo(targetIndex, attacker)) != 0L
      case PieceType.Knight =>
        (bitboards.knights(attacker) & Bitboards.knightAttacks(targetIndex)) != 0L
      case PieceType.Bishop =>
        sliderAttacksTarget(bitboards.occupied, bitboards.bishops(attacker), targetIndex, diagonal = true)
      case PieceType.Rook =>
        sliderAttacksTarget(bitboards.occupied, bitboards.rooks(attacker), targetIndex, diagonal = false)
      case PieceType.Queen =>
        sliderAttacksTarget(bitboards.occupied, bitboards.queens(attacker), targetIndex, diagonal = true) ||
          sliderAttacksTarget(bitboards.occupied, bitboards.queens(attacker), targetIndex, diagonal = false)
      case PieceType.King =>
        (bitboards.kings(attacker) & Bitboards.kingAttacks(targetIndex)) != 0L

  private def pawnAttackersTo(targetIndex: Int, attacker: Color): Long =
    val targetFile = targetIndex & 7
    val targetRank = targetIndex >>> 3
    val fromRank = if attacker == Color.White then targetRank - 1 else targetRank + 1
    if fromRank < 0 || fromRank >= 8 then 0L
    else
      var bits = 0L
      if targetFile > 0 then bits |= Bitboards.mask(fromRank * 8 + targetFile - 1)
      if targetFile < 7 then bits |= Bitboards.mask(fromRank * 8 + targetFile + 1)
      bits

  private def pawnAttacksFrom(fromIndex: Int, color: Color): Long =
    val file = fromIndex & 7
    val rank = fromIndex >>> 3
    val targetRank = if color == Color.White then rank + 1 else rank - 1
    if targetRank < 0 || targetRank >= 8 then 0L
    else
      var bits = 0L
      if file > 0 then bits |= Bitboards.mask(targetRank * 8 + file - 1)
      if file < 7 then bits |= Bitboards.mask(targetRank * 8 + file + 1)
      bits

  private val diagonalDirections: List[Int] = List(9, 7, -7, -9)
  private val orthogonalDirections: List[Int] = List(1, -1, 8, -8)

  private def slidingAttacksFrom(occupied: Long, fromIndex: Int, directions: List[Int]): Long =
    var attacks = 0L
    directions.foreach { step =>
      var current = fromIndex + step
      var previous = fromIndex
      var blocked = false
      while !blocked && current >= 0 && current < 64 && sameRay(previous, current, step) do
        val bit = Bitboards.mask(current)
        attacks |= bit
        if (occupied & bit) != 0L then blocked = true
        previous = current
        current += step
    }
    attacks

  private def sameRay(previous: Int, current: Int, step: Int): Boolean =
    val previousFile = previous & 7
    val currentFile = current & 7
    step match
      case 1 | -7 | 9  => currentFile == previousFile + 1
      case -1 | 7 | -9 => currentFile == previousFile - 1
      case 8 | -8      => currentFile == previousFile
      case _           => false

  private def isRookLine(df: Int, dr: Int): Boolean =
    (df == 0 && dr != 0) || (dr == 0 && df != 0)

  private def isBishopLine(df: Int, absDf: Int, absDr: Int): Boolean =
    absDf == absDr && df != 0

  private def isQueenLine(df: Int, dr: Int, absDf: Int): Boolean =
    isRookLine(df, dr) || isBishopLine(df, absDf, Math.abs(dr))
