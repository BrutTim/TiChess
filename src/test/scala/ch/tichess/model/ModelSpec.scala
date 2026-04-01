package ch.tichess.model

import org.scalatest.funsuite.AnyFunSuite

final class ModelSpec extends AnyFunSuite:

  test("Color.other flips") {
    assert(Color.White.other == Color.Black)
    assert(Color.Black.other == Color.White)
  }

  test("Pos.fromAlgebraic parses valid and rejects invalid") {
    assert(Pos.fromAlgebraic("a1") == Right(Pos(0, 0)))
    assert(Pos.fromAlgebraic("h8") == Right(Pos(7, 7)))
    assert(Pos.fromAlgebraic("e2") == Right(Pos(4, 1)))
    assert(Pos.fromAlgebraic("") == Left("Position must have length 2 (e.g. e2)."))
    assert(Pos.fromAlgebraic("e9") == Left("Position out of bounds."))
    assert(Pos.fromAlgebraic("z1") == Left("Position out of bounds."))
  }

  test("Pos.+ adds delta") {
    assert(Pos(1, 2) + (2, 3) == Pos(3, 5))
  }

  test("PieceType promotion parser accepts promotable roles and rejects others") {
    assert(PromotionRole.fromPromotionChar("q") == Right(PromotionRole.Queen))
    assert(PromotionRole.fromPromotionChar("R") == Right(PromotionRole.Rook))
    assert(PromotionRole.fromPromotionChar("b") == Right(PromotionRole.Bishop))
    assert(PromotionRole.fromPromotionChar("n") == Right(PromotionRole.Knight))
    assert(PromotionRole.fromPromotionChar("k") == Left("Promotion must be one of: q, r, b, n."))
  }

  test("Board.initial has 32 pieces and correct corners") {
    val b = Board.initial
    assert(b.allPieces.size == 32)
    assert(b.pieceAt(Pos(0, 0)).contains(Piece(Color.White, PieceType.Rook)))
    assert(b.pieceAt(Pos(7, 7)).contains(Piece(Color.Black, PieceType.Rook)))
    assert(!b.isEmpty(Pos(4, 0)))
    assert(b.isEmpty(Pos(4, 4)))
  }

  test("Board.movePiece moves and captures") {
    val b0 = Board.empty
      .copy(pieces =
        Map(
          Pos(0, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      )

    val moved = b0.movePiece(Move(Pos(0, 0), Pos(0, 7))).toOption.get
    assert(moved.pieceAt(Pos(0, 0)).isEmpty)
    assert(moved.pieceAt(Pos(0, 7)).contains(Piece(Color.White, PieceType.Rook)))
  }

  test("Board.movePiece rejects when no source piece; Board.removeAt removes") {
    val b = Board.empty.copy(pieces = Map(Pos(0, 0) -> Piece(Color.White, PieceType.King)))
    assert(b.movePiece(Move(Pos(1, 1), Pos(1, 2))) == Left("No piece at source position."))
    val removed = b.removeAt(Pos(0, 0))
    assert(removed.pieceAt(Pos(0, 0)).isEmpty)
  }

  test("Rules.validateMove rejects same-square and empty source") {
    val g = Game.initial
    val from = Pos(4, 1)
    assert(Rules.validateMove(g.board, Color.White, Move(from, from)).left.exists(_.nonEmpty))
    assert(Rules.validateMove(g.board, Color.White, Move(Pos(4, 4), Pos(4, 5))) == Left("No piece at source position."))
  }

  test("Rules.validateMove rejects moving opponent piece and capturing own piece") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(0, 0) -> Piece(Color.White, PieceType.King),
        Pos(1, 1) -> Piece(Color.Black, PieceType.King),
        Pos(2, 2) -> Piece(Color.White, PieceType.Bishop)
      )
    )

    assert(Rules.validateMove(b, Color.White, Move(Pos(1, 1), Pos(1, 2))) == Left("Not your piece."))
    assert(Rules.validateMove(b, Color.White, Move(Pos(0, 0), Pos(2, 2))) == Left("Cannot capture your own piece."))
  }

  test("King moves one step; rejects longer") {
    val b = Board.empty.copy(pieces = Map(Pos(4, 4) -> Piece(Color.White, PieceType.King)))
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 4), Pos(5, 5))).isRight)
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 4), Pos(6, 4))) == Left("Illegal king move."))
  }

  test("Knight jumps; rejects non-L") {
    val b = Board.empty.copy(pieces = Map(Pos(4, 4) -> Piece(Color.White, PieceType.Knight)))
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 4), Pos(6, 5))).isRight)
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 4), Pos(5, 5))) == Left("Illegal knight move."))
  }

  test("Rook is blocked by pieces") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(0, 0) -> Piece(Color.White, PieceType.Rook),
        Pos(0, 3) -> Piece(Color.White, PieceType.Pawn)
      )
    )
    assert(Rules.validateMove(b, Color.White, Move(Pos(0, 0), Pos(0, 7))) == Left("Illegal rook move."))
    assert(Rules.validateMove(b, Color.White, Move(Pos(0, 0), Pos(0, 2))).isRight)
  }

  test("Bishop diagonal move respects blocking") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(2, 0) -> Piece(Color.White, PieceType.Bishop),
        Pos(3, 1) -> Piece(Color.Black, PieceType.Pawn)
      )
    )
    assert(Rules.validateMove(b, Color.White, Move(Pos(2, 0), Pos(4, 2))) == Left("Illegal bishop move."))
    assert(Rules.validateMove(b, Color.White, Move(Pos(2, 0), Pos(3, 1))).isRight)
    assert(Rules.validateMove(b, Color.White, Move(Pos(2, 0), Pos(2, 1))) == Left("Illegal bishop move."))
  }

  test("Queen supports rook/bishop lines and path blocking") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(4, 4) -> Piece(Color.White, PieceType.Queen),
        Pos(4, 6) -> Piece(Color.Black, PieceType.Pawn)
      )
    )
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 4), Pos(4, 7))) == Left("Illegal queen move."))
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 4), Pos(4, 6))).isRight)
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 4), Pos(6, 5))) == Left("Illegal queen move."))
  }

  test("Pawn forward, double-step, and diagonal capture rules") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(4, 1) -> Piece(Color.White, PieceType.Pawn),
        Pos(5, 2) -> Piece(Color.Black, PieceType.Knight),
        Pos(4, 2) -> Piece(Color.Black, PieceType.Pawn) // blocks forward
      )
    )
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 1), Pos(4, 2))) == Left("Illegal pawn move."))
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 1), Pos(4, 3))) == Left("Illegal pawn move."))
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 1), Pos(5, 2))).isRight)
    assert(Rules.validateMove(b, Color.White, Move(Pos(4, 1), Pos(3, 2))) == Left("Illegal pawn move."))
  }

  test("Pawn double-step works when clear; black pawn direction") {
    val bW = Board.empty.copy(pieces = Map(Pos(4, 1) -> Piece(Color.White, PieceType.Pawn)))
    assert(Rules.validateMove(bW, Color.White, Move(Pos(4, 1), Pos(4, 3))).isRight)

    val bB = Board.empty.copy(pieces = Map(Pos(4, 6) -> Piece(Color.Black, PieceType.Pawn)))
    assert(Rules.validateMove(bB, Color.Black, Move(Pos(4, 6), Pos(4, 4))).isRight)
    assert(Rules.validateMove(bB, Color.Black, Move(Pos(4, 6), Pos(4, 7))) == Left("Illegal pawn move."))
  }

  test("Promotion validation accepts promotable roles and rejects invalid promotion usage") {
    val whitePawn = Board.empty.copy(pieces = Map(Pos(0, 6) -> Piece(Color.White, PieceType.Pawn)))
    assert(Rules.validateMove(whitePawn, Color.White, Move(Pos(0, 6), Pos(0, 7), Some(PromotionRole.Queen))).isRight)

    val rookBoard = Board.empty.copy(pieces = Map(Pos(0, 0) -> Piece(Color.White, PieceType.Rook)))
    assert(
      Rules.validateMove(rookBoard, Color.White, Move(Pos(0, 0), Pos(0, 7), Some(PromotionRole.Queen))) ==
        Left("Promotion is only allowed for pawns reaching the back rank.")
    )
  }

  test("Game.applyMove applies move and flips side; rejects invalid") {
    val g0 = Game.initial
    val ok = g0.applyMove(Move(Pos(4, 1), Pos(4, 3))).toOption.get
    assert(ok.sideToMove == Color.Black)

    val bad = g0.applyMove(Move(Pos(4, 1), Pos(4, 7)))
    assert(bad.isLeft)
  }

  test("Game.applyMove promotes pawns, defaults to queen, and exposes all promotion roles in legalMoves") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(4, 6) -> Piece(Color.White, PieceType.Pawn),
        Pos(7, 7) -> Piece(Color.Black, PieceType.King),
        Pos(0, 0) -> Piece(Color.White, PieceType.King)
      )
    )
    val game = Game(board, Color.White)

    val missingPromotion = game.applyMove(Move(Pos(4, 6), Pos(4, 7)))
    assert(missingPromotion == Left("Promotion required: choose q, r, b, or n."))

    val knightPromotion = game.applyMove(Move(Pos(4, 6), Pos(4, 7), Some(PromotionRole.Knight))).toOption.get
    assert(knightPromotion.board.pieceAt(Pos(4, 7)).contains(Piece(Color.White, PieceType.Knight)))

    val queenPromotion = game.applyMove(Move(Pos(4, 6), Pos(4, 7), Some(PromotionRole.Queen))).toOption.get
    assert(queenPromotion.board.pieceAt(Pos(4, 7)).contains(Piece(Color.White, PieceType.Queen)))

    val promotionMoves = game.legalMoves.filter(m => m.from == Pos(4, 6) && m.to == Pos(4, 7)).flatMap(_.promotion).toSet
    assert(promotionMoves == PromotionRole.values.toSet)
  }

  test("Rules.isInCheck detects rook and knight attacks") {
    val rookCheck = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook)
      )
    )
    assert(Rules.isInCheck(rookCheck, Color.White))
    assert(!Rules.isInCheck(rookCheck, Color.Black))

    val knightCheck = Board.empty.copy(
      pieces = Map(
        Pos(4, 4) -> Piece(Color.White, PieceType.King),
        Pos(5, 6) -> Piece(Color.Black, PieceType.Knight)
      )
    )
    assert(Rules.isInCheck(knightCheck, Color.White))

    val queenCheck = Board.empty.copy(
      pieces = Map(
        Pos(4, 4) -> Piece(Color.White, PieceType.King),
        Pos(1, 1) -> Piece(Color.Black, PieceType.Queen)
      )
    )
    assert(Rules.isInCheck(queenCheck, Color.White))

    val bishopCheck = Board.empty.copy(
      pieces = Map(
        Pos(4, 4) -> Piece(Color.White, PieceType.King),
        Pos(1, 1) -> Piece(Color.Black, PieceType.Bishop)
      )
    )
    assert(Rules.isInCheck(bishopCheck, Color.White))
  }

  test("Rules.isInCheck false when line is blocked and when king missing") {
    val blocked = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook),
        Pos(4, 3) -> Piece(Color.White, PieceType.Pawn)
      )
    )
    assert(!Rules.isInCheck(blocked, Color.White))

    val noWhiteKing = Board.empty.copy(pieces = Map(Pos(0, 0) -> Piece(Color.Black, PieceType.King)))
    assert(!Rules.isInCheck(noWhiteKing, Color.White))

    val whitePawnCheck = Board.empty.copy(
      pieces = Map(
        Pos(4, 4) -> Piece(Color.Black, PieceType.King),
        Pos(3, 3) -> Piece(Color.White, PieceType.Pawn)
      )
    )
    assert(Rules.isInCheck(whitePawnCheck, Color.Black))

    val kingAdjCheck = Board.empty.copy(
      pieces = Map(
        Pos(4, 4) -> Piece(Color.White, PieceType.King),
        Pos(5, 5) -> Piece(Color.Black, PieceType.King)
      )
    )
    assert(Rules.isInCheck(kingAdjCheck, Color.White))
  }

  test("When in check, unrelated move is illegal") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),  // e1
        Pos(0, 0) -> Piece(Color.White, PieceType.Rook),  // a1
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook),  // e8 gives check
        Pos(7, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val g = Game(b, Color.White)
    val res = g.applyMove(Move(Pos(0, 0), Pos(0, 1))) // unrelated rook move
    assert(res == Left("Illegal move: king would remain in check."))
  }

  test("Check can be resolved by capture, block, or king move to safe square") {
    val captureBoard = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(4, 1) -> Piece(Color.White, PieceType.Queen),
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook),
        Pos(7, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val captureGame = Game(captureBoard, Color.White)
    assert(captureGame.applyMove(Move(Pos(4, 1), Pos(4, 7))).isRight)

    val blockBoard = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),   // e1
        Pos(2, 1) -> Piece(Color.White, PieceType.Bishop), // c2 can block on e4
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook),   // e8
        Pos(7, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val blockGame = Game(blockBoard, Color.White)
    assert(blockGame.applyMove(Move(Pos(2, 1), Pos(4, 3))).isRight) // c2 -> e4 blocks line

    val kingMoveBoard = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King), // e1
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook), // e8 check
        Pos(7, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val kingMoveGame = Game(kingMoveBoard, Color.White)
    assert(kingMoveGame.applyMove(Move(Pos(4, 0), Pos(3, 0))).isRight) // e1 -> d1
  }

  test("King cannot move onto attacked square") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King), // e1
        Pos(3, 7) -> Piece(Color.Black, PieceType.Rook), // d8 attacks d1
        Pos(7, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val g = Game(b, Color.White)
    val res = g.applyMove(Move(Pos(4, 0), Pos(3, 0))) // e1 -> d1 into attack
    assert(res == Left("Illegal move: king would remain in check."))
  }

  test("Game.isCheckmate is true when in check and no legal moves") {
    val mateBoard = Board.empty.copy(
      pieces = Map(
        Pos(0, 7) -> Piece(Color.Black, PieceType.King), // a8
        Pos(1, 6) -> Piece(Color.White, PieceType.Queen), // b7 gives check
        Pos(2, 5) -> Piece(Color.White, PieceType.King) // c6 protects b7
      )
    )
    val g = Game(mateBoard, Color.Black)
    assert(g.isInCheck)
    assert(g.legalMoves.isEmpty)
    assert(g.isCheckmate)
  }

  test("Game.isCheckmate is false when check can be escaped") {
    val checkBoard = Board.empty.copy(
      pieces = Map(
        Pos(0, 7) -> Piece(Color.Black, PieceType.King), // a8
        Pos(0, 0) -> Piece(Color.White, PieceType.Rook), // a1 check on file
        Pos(7, 0) -> Piece(Color.White, PieceType.King)
      )
    )
    val g = Game(checkBoard, Color.Black)
    assert(g.isInCheck)
    assert(g.legalMoves.nonEmpty) // e.g. a8 -> b8
    assert(!g.isCheckmate)
  }

  test("Game.isCheckmate is false when not in check") {
    val g = Game.initial
    assert(!g.isInCheck)
    assert(g.legalMoves.nonEmpty)
    assert(!g.isCheckmate)
  }

  test("Rules helpers: sign and squaresBetweenExclusive") {
    assert(Rules.sign(0) == 0)
    assert(Rules.sign(5) == 1)
    assert(Rules.sign(-2) == -1)

    assert(Rules.squaresBetweenExclusive(Pos(0, 0), Pos(0, 3)) == List(Pos(0, 1), Pos(0, 2)))
    assert(Rules.squaresBetweenExclusive(Pos(0, 3), Pos(0, 0)) == List(Pos(0, 2), Pos(0, 1)))
    assert(Rules.squaresBetweenExclusive(Pos(0, 0), Pos(3, 3)) == List(Pos(1, 1), Pos(2, 2)))
    assert(Rules.squaresBetweenExclusive(Pos(0, 0), Pos(3, 2)).isEmpty) // not a straight line

    val b = Board.empty.copy(pieces = Map(Pos(0, 1) -> Piece(Color.White, PieceType.Pawn)))
    assert(!Rules.clearPath(b, Pos(0, 0), Pos(0, 3)))
    assert(Rules.findKing(Board.empty.copy(pieces = Map(Pos(3, 3) -> Piece(Color.White, PieceType.King))), Color.White).contains(Pos(3, 3)))
  }
