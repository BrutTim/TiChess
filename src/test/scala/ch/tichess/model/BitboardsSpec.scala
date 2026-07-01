package ch.tichess.model

import org.scalatest.funsuite.AnyFunSuite

final class BitboardsSpec extends AnyFunSuite:

  test("Bitboards map positions to a1-based bit indexes and masks") {
    assert(Bitboards.index(Pos(0, 0)) == 0)
    assert(Bitboards.index(Pos(7, 0)) == 7)
    assert(Bitboards.index(Pos(0, 7)) == 56)
    assert(Bitboards.index(Pos(7, 7)) == 63)

    assert(Bitboards.pos(0) == Pos(0, 0))
    assert(Bitboards.pos(63) == Pos(7, 7))
    assert(Bitboards.mask(Pos(0, 0)) == 1L)
    assert(Bitboards.mask(Pos(7, 7)) == (1L << 63))
  }

  test("Bitboards.fromBoard builds piece, color, occupancy and piece lookup masks") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(0, 1) -> Piece(Color.White, PieceType.Pawn),
        Pos(1, 0) -> Piece(Color.White, PieceType.Knight),
        Pos(4, 7) -> Piece(Color.Black, PieceType.King),
        Pos(7, 6) -> Piece(Color.Black, PieceType.Pawn),
        Pos(3, 7) -> Piece(Color.Black, PieceType.Queen)
      )
    )

    val bitboards = Bitboards.fromBoard(board)

    assert(Bitboards.popCount(bitboards.whitePieces) == 3)
    assert(Bitboards.popCount(bitboards.blackPieces) == 3)
    assert(Bitboards.popCount(bitboards.occupied) == 6)
    assert(bitboards.pieceAt(Pos(0, 1)).contains(Piece(Color.White, PieceType.Pawn)))
    assert(bitboards.pieceAt(Pos(3, 7)).contains(Piece(Color.Black, PieceType.Queen)))
    assert(bitboards.pieceAt(Pos(2, 2)).isEmpty)
    assert(bitboards.kingSquare(Color.White).contains(Pos(4, 0)))
    assert(bitboards.kingSquare(Color.Black).contains(Pos(4, 7)))
    assert(bitboards.pieceList(Color.White).map(_._2.kind).toSet == Set(PieceType.King, PieceType.Knight, PieceType.Pawn))
    assert((bitboards.empty & bitboards.occupied) == 0L)
  }

  test("pieceAt covers every piece bitboard") {
    val pieces =
      PieceType.values.zipWithIndex.flatMap { case (kind, idx) =>
        Seq(
          Pos(idx, 0) -> Piece(Color.White, kind),
          Pos(idx, 7) -> Piece(Color.Black, kind)
        )
      }.toMap
    val bitboards = Bitboards.fromBoard(Board(pieces))

    PieceType.values.zipWithIndex.foreach { case (kind, idx) =>
      assert(bitboards.pieceAt(Pos(idx, 0)).contains(Piece(Color.White, kind)))
      assert(bitboards.pieceAt(Pos(idx, 7)).contains(Piece(Color.Black, kind)))
    }
  }

  test("Board keeps native bitboards in sync across remove, set and move operations") {
    val board = Board.empty
      .setAt(Pos(4, 0), Piece(Color.White, PieceType.King))
      .setAt(Pos(0, 0), Piece(Color.White, PieceType.Rook))
      .setAt(Pos(0, 7), Piece(Color.Black, PieceType.Rook))

    assert(board.copy() == board)
    assert(!board.equals("not a board"))
    assert(board.toString.startsWith("Board("))
    assert(board.bitboards.pieceAt(Pos(0, 0)).contains(Piece(Color.White, PieceType.Rook)))
    assert(board.removeAt(Pos(7, 7)) eq board)

    val moved = board.movePiece(Move(Pos(0, 0), Pos(0, 7))).toOption.get
    assert(moved.pieceAt(Pos(0, 0)).isEmpty)
    assert(moved.bitboards.pieceAt(Pos(0, 0)).isEmpty)
    assert(moved.bitboards.pieceAt(Pos(0, 7)).contains(Piece(Color.White, PieceType.Rook)))
    assert((moved.bitboards.blackRooks & Bitboards.mask(Pos(0, 7))) == 0L)

    val promoted = moved.setAt(Pos(0, 7), Piece(Color.White, PieceType.Queen))
    assert(promoted.bitboards.pieceAt(Pos(0, 7)).contains(Piece(Color.White, PieceType.Queen)))
    assert((promoted.bitboards.whiteRooks & Bitboards.mask(Pos(0, 7))) == 0L)

    val removed = promoted.removeAt(Pos(0, 7))
    assert(removed.pieceAt(Pos(0, 7)).isEmpty)
    assert(removed.bitboards.pieceAt(Pos(0, 7)).isEmpty)
  }

  test("foreachSetBit scans all set bits from least to most significant") {
    val seen = scala.collection.mutable.ListBuffer.empty[Int]
    Bitboards.foreachSetBit(Bitboards.mask(0) | Bitboards.mask(9) | Bitboards.mask(63))(seen += _)

    assert(seen.toList == List(0, 9, 63))
  }

  test("BitboardAttacks detects leaper, pawn and sliding attacks with blockers") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(1, 0) -> Piece(Color.White, PieceType.Rook),
        Pos(2, 0) -> Piece(Color.White, PieceType.Pawn),
        Pos(4, 7) -> Piece(Color.Black, PieceType.King),
        Pos(1, 7) -> Piece(Color.Black, PieceType.Rook),
        Pos(2, 2) -> Piece(Color.Black, PieceType.Knight),
        Pos(3, 1) -> Piece(Color.Black, PieceType.Pawn),
        Pos(7, 3) -> Piece(Color.Black, PieceType.Bishop)
      )
    )
    val bitboards = Bitboards.fromBoard(board)

    assert(BitboardAttacks.isAttackedBy(bitboards, Color.Black, Pos(4, 0)))
    assert(BitboardAttacks.attacksSquare(bitboards, Pos(2, 2), Piece(Color.Black, PieceType.Knight), Pos(4, 1)))
    assert(BitboardAttacks.attacksSquare(bitboards, Pos(3, 1), Piece(Color.Black, PieceType.Pawn), Pos(4, 0)))
    assert(BitboardAttacks.attacksSquare(bitboards, Pos(0, 1), Piece(Color.White, PieceType.Pawn), Pos(1, 2)))
    assert(BitboardAttacks.attacksSquare(bitboards, Pos(4, 0), Piece(Color.White, PieceType.King), Pos(4, 1)))
    assert(!BitboardAttacks.attacksSquare(bitboards, Pos(4, 0), Piece(Color.White, PieceType.King), Pos(4, 2)))
    assert(BitboardAttacks.attacksSquare(Bitboards.empty, Pos(3, 0), Piece(Color.White, PieceType.Queen), Pos(3, 3)))
    assert(!BitboardAttacks.attacksSquare(Bitboards.empty, Pos(3, 0), Piece(Color.White, PieceType.Queen), Pos(5, 1)))
    assert(!BitboardAttacks.attacksSquare(bitboards, Pos(0, 6), Piece(Color.Black, PieceType.Pawn), Pos(1, 7)))
    assert(BitboardAttacks.attacksSquare(bitboards, Pos(7, 3), Piece(Color.Black, PieceType.Bishop), Pos(4, 0)))
    assert(!BitboardAttacks.attacksSquare(bitboards, Pos(4, 0), Piece(Color.White, PieceType.King), Pos(4, 0)))
    assert(!BitboardAttacks.attacksSquare(bitboards, Bitboards.index(Pos(4, 0)), Piece(Color.White, PieceType.King), Bitboards.index(Pos(4, 0))))
    assert(!BitboardAttacks.attacksSquare(bitboards, Pos(1, 0), Piece(Color.White, PieceType.Rook), Pos(4, 0)))
    assert(!BitboardAttacks.clearPath(bitboards, Pos(1, 0), Pos(4, 0)))
    assert(BitboardAttacks.clearPath(bitboards, Pos(7, 3), Pos(4, 0)))
  }

  test("Bitboard check detection matches missing-king and blocked-line behavior") {
    assert(!BitboardAttacks.isInCheck(Bitboards.empty, Color.White))

    val blocked = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(4, 1) -> Piece(Color.White, PieceType.Pawn),
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook)
      )
    )
    val open = blocked.copy(pieces = blocked.pieces - Pos(4, 1))

    assert(!Rules.isInCheck(blocked, Color.White))
    assert(Rules.isInCheck(open, Color.White))
    assert(BitboardAttacks.isInCheck(open.bitboards, Color.White))
  }

  test("BitboardAttacks attacksFrom covers piece kinds, sliders and pawn board edges") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(3, 3) -> Piece(Color.White, PieceType.Queen),
        Pos(5, 5) -> Piece(Color.White, PieceType.Bishop),
        Pos(3, 6) -> Piece(Color.Black, PieceType.Pawn),
        Pos(6, 3) -> Piece(Color.Black, PieceType.Rook)
      )
    )
    val bitboards = board.bitboards
    val from = Bitboards.index(Pos(3, 3))

    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.King)) & Bitboards.mask(Pos(4, 4))) != 0L)
    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.Knight)) & Bitboards.mask(Pos(4, 5))) != 0L)
    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.Pawn)) & Bitboards.mask(Pos(2, 4))) != 0L)
    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.Bishop)) & Bitboards.mask(Pos(5, 5))) != 0L)
    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.Bishop)) & Bitboards.mask(Pos(6, 6))) == 0L)
    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.Rook)) & Bitboards.mask(Pos(3, 6))) != 0L)
    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.Rook)) & Bitboards.mask(Pos(3, 7))) == 0L)
    assert((BitboardAttacks.attacksFrom(bitboards, from, Piece(Color.White, PieceType.Queen)) & Bitboards.mask(Pos(6, 3))) != 0L)
    assert(BitboardAttacks.attacksFrom(Bitboards.empty, Bitboards.index(Pos(0, 7)), Piece(Color.White, PieceType.Pawn)) == 0L)
    assert(BitboardAttacks.attacksFrom(Bitboards.empty, Bitboards.index(Pos(7, 0)), Piece(Color.Black, PieceType.Pawn)) == 0L)
  }

  test("BitboardAttacks kind-specific queries cover every attacker family") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(4, 4) -> Piece(Color.White, PieceType.King),
        Pos(5, 3) -> Piece(Color.Black, PieceType.Pawn),
        Pos(2, 3) -> Piece(Color.Black, PieceType.Knight),
        Pos(1, 1) -> Piece(Color.Black, PieceType.Bishop),
        Pos(4, 0) -> Piece(Color.Black, PieceType.Rook),
        Pos(0, 4) -> Piece(Color.Black, PieceType.Queen),
        Pos(5, 5) -> Piece(Color.Black, PieceType.King)
      )
    )
    val bitboards = board.bitboards

    assert(BitboardAttacks.isAttackedByKind(bitboards, Color.Black, PieceType.Pawn, Pos(4, 2)))
    assert(BitboardAttacks.isAttackedByKind(bitboards, Color.Black, PieceType.Knight, Pos(4, 4)))
    assert(BitboardAttacks.isAttackedByKind(bitboards, Color.Black, PieceType.Bishop, Pos(4, 4)))
    assert(BitboardAttacks.isAttackedByKind(bitboards, Color.Black, PieceType.Rook, Pos(4, 4)))
    assert(BitboardAttacks.isAttackedByKind(bitboards, Color.Black, PieceType.Queen, Pos(4, 4)))
    assert(BitboardAttacks.isAttackedByKind(bitboards, Color.Black, PieceType.King, Pos(4, 4)))
    assert(!BitboardAttacks.isAttackedByKind(bitboards, Color.White, PieceType.Queen, Pos(4, 4)))
    assert(BitboardAttacks.clearPath(Bitboards.empty.occupied, Bitboards.index(Pos(0, 0)), Bitboards.index(Pos(0, 1))))
  }

  test("BitboardAttacks sameRay defensive fallback rejects unknown steps") {
    val method = BitboardAttacks.getClass.getDeclaredMethod("sameRay", classOf[Int], classOf[Int], classOf[Int])
    method.setAccessible(true)

    assert(method.invoke(BitboardAttacks, Int.box(0), Int.box(1), Int.box(2)) == java.lang.Boolean.FALSE)
  }
