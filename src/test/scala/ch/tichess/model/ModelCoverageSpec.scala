package ch.tichess.model

import org.scalatest.funsuite.AnyFunSuite

final class ModelCoverageSpec extends AnyFunSuite:

  test("Rules.validateMove covers castling failure branches") {
    val outOfCheckBoard = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
        Pos(4, 7) -> Piece(Color.Black, PieceType.Rook),
        Pos(0, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val outOfCheckGame = Game(outOfCheckBoard, Color.White)
    assert(Rules.validateMove(outOfCheckGame, Move(Pos(4, 0), Pos(6, 0))) == Left("Cannot castle out of check."))

    val lostRightsGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White,
      CastlingRights(whiteKingside = false, whiteQueenside = true, blackKingside = true, blackQueenside = true)
    )
    assert(Rules.validateMove(lostRightsGame, Move(Pos(4, 0), Pos(6, 0))) == Left("Castling rights lost."))

    val blockedPathGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(5, 0) -> Piece(Color.White, PieceType.Bishop),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    )
    assert(Rules.validateMove(blockedPathGame, Move(Pos(4, 0), Pos(6, 0))) == Left("Castling path not clear."))

    val throughCheckGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(5, 7) -> Piece(Color.Black, PieceType.Rook),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    )
    assert(Rules.validateMove(throughCheckGame, Move(Pos(4, 0), Pos(6, 0))) == Left("Cannot castle through check."))
  }

  test("Game.applyMove covers castling execution, en passant, and castling-right updates") {
    val castleKingside = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    ).applyMove(Move(Pos(4, 0), Pos(6, 0))).toOption.get

    assert(castleKingside.board.pieceAt(Pos(6, 0)).contains(Piece(Color.White, PieceType.King)))
    assert(castleKingside.board.pieceAt(Pos(5, 0)).contains(Piece(Color.White, PieceType.Rook)))
    assert(castleKingside.castlingRights.whiteKingside == false)
    assert(castleKingside.castlingRights.whiteQueenside == false)

    val castleQueenside = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(0, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    ).applyMove(Move(Pos(4, 0), Pos(2, 0))).toOption.get

    assert(castleQueenside.board.pieceAt(Pos(2, 0)).contains(Piece(Color.White, PieceType.King)))
    assert(castleQueenside.board.pieceAt(Pos(3, 0)).contains(Piece(Color.White, PieceType.Rook)))

    val rookMove = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(0, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    ).applyMove(Move(Pos(0, 0), Pos(0, 1))).toOption.get
    assert(!rookMove.castlingRights.whiteQueenside)

    val rookMoveKingside = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    ).applyMove(Move(Pos(7, 0), Pos(7, 1))).toOption.get
    assert(!rookMoveKingside.castlingRights.whiteKingside)

    val rookCapture = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(0, 5) -> Piece(Color.White, PieceType.Rook),
          Pos(0, 7) -> Piece(Color.Black, PieceType.Rook),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    ).applyMove(Move(Pos(0, 5), Pos(0, 7))).toOption.get
    assert(!rookCapture.castlingRights.blackQueenside)

    val rookCaptureKingside = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 5) -> Piece(Color.White, PieceType.Rook),
          Pos(7, 7) -> Piece(Color.Black, PieceType.Rook),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    ).applyMove(Move(Pos(7, 5), Pos(7, 7))).toOption.get
    assert(!rookCaptureKingside.castlingRights.blackKingside)

    val blackRookMove = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(7, 7) -> Piece(Color.Black, PieceType.Rook),
          Pos(4, 7) -> Piece(Color.Black, PieceType.King),
          Pos(4, 0) -> Piece(Color.White, PieceType.King)
        )
      ),
      Color.Black
    ).applyMove(Move(Pos(7, 7), Pos(7, 6))).toOption.get
    assert(!blackRookMove.castlingRights.blackKingside)

    val whiteRookCapturedOnKingside = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 5) -> Piece(Color.Black, PieceType.Rook),
          Pos(4, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.Black
    ).applyMove(Move(Pos(7, 5), Pos(7, 0))).toOption.get
    assert(!whiteRookCapturedOnKingside.castlingRights.whiteKingside)

    val enPassantGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(0, 0) -> Piece(Color.White, PieceType.King),
          Pos(4, 4) -> Piece(Color.White, PieceType.Pawn),
          Pos(3, 4) -> Piece(Color.Black, PieceType.Pawn),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White,
      enPassantTarget = Some(Pos(3, 5))
    )

    assert(Rules.validateMove(enPassantGame, Move(Pos(4, 4), Pos(3, 5))).isRight)
    val enPassantApplied = enPassantGame.applyMove(Move(Pos(4, 4), Pos(3, 5))).toOption.get
    assert(enPassantApplied.board.pieceAt(Pos(3, 5)).contains(Piece(Color.White, PieceType.Pawn)))
    assert(enPassantApplied.board.pieceAt(Pos(3, 4)).isEmpty)
  }

  test("Game.legalMoves exposes pseudo-generated special moves safely") {
    assert(Game.initial.legalMoves.size == 20)

    val castlingGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(0, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(4, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    )
    val castlingMoves = castlingGame.legalMoves.toSet
    assert(castlingMoves.contains(Move(Pos(4, 0), Pos(6, 0))))
    assert(castlingMoves.contains(Move(Pos(4, 0), Pos(2, 0))))

    val enPassantGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(0, 0) -> Piece(Color.White, PieceType.King),
          Pos(4, 4) -> Piece(Color.White, PieceType.Pawn),
          Pos(3, 4) -> Piece(Color.Black, PieceType.Pawn),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White,
      enPassantTarget = Some(Pos(3, 5))
    )
    assert(enPassantGame.legalMoves.contains(Move(Pos(4, 4), Pos(3, 5))))
  }

  test("Fen parsers cover optional castling, en-passant, halfmove, and fullmove fields") {
    val castlingOnly = "4k3/8/8/8/8/8/8/4K3 w KQ"
    val withEnPassant = "4k3/8/8/8/8/8/8/4K3 w KQ e3"
    val withHalfmove = "4k3/8/8/8/8/8/8/4K3 w KQ e3 7"
    val withFullmove = "4k3/8/8/8/8/8/8/4K3 w KQ e3 7 12"

    List(ParserCombinatorsFenParser, RegexFenParser).foreach { parser =>
      assert(Fen.parseWith(parser, castlingOnly).toOption.get.castlingRights.whiteKingside)
      assert(Fen.parseWith(parser, withEnPassant).toOption.get.enPassantTarget.contains(Pos(4, 2)))
      assert(Fen.parseWith(parser, withHalfmove).toOption.get.halfMoveClock == 7)
      assert(Fen.parseWith(parser, withFullmove).toOption.get.fullMoveNumber == 12)
    }
  }

  test("Fen parsing and Pos parsing cover remaining validation branches") {
    assert(Pos.fromAlgebraic("aa") == Left("Position out of bounds."))
    assert(Pos(0, 0).toAlgebraic == "a1")
    assert(Pos(7, 7).toAlgebraic == "h8")

    val invalidEp = Fen.parse("4k3/8/8/8/8/8/8/4K3 w - z9 0 1").toOption.get
    assert(invalidEp.enPassantTarget.isEmpty)

    val gameWithEp = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(4, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.Black,
      enPassantTarget = Some(Pos(2, 2))
    )
    assert(Fen.encode(gameWithEp).contains(" c3 "))
  }

  test("Pgn.encode covers inferred draw result and default result parameter") {
    val stalemateStart = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(2, 5) -> Piece(Color.White, PieceType.King),
          Pos(2, 6) -> Piece(Color.White, PieceType.Queen),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.Black
    )

    val encoded = PgnSupport.encode(stalemateStart, Vector.empty)
    assert(encoded.contains("""[Result "1/2-1/2"]"""))
  }
