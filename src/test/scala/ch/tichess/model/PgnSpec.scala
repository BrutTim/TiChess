package ch.tichess.model

import org.scalatest.funsuite.AnyFunSuite

final class PgnSpec extends AnyFunSuite:

  private val moves = Vector(
    Move(Pos(4, 1), Pos(4, 3)),
    Move(Pos(4, 6), Pos(4, 4)),
    Move(Pos(6, 0), Pos(5, 2))
  )

  private val moveTextPgn =
    """[Event "TiChess Game"]
      |[Site "Local"]
      |[Date "2026.04.14"]
      |[Round "-"]
      |[White "White"]
      |[Black "Black"]
      |[Result "*"]
      |
      |1. e2e4 e7e5 2. g1f3 *""".stripMargin

  private def replay(start: Game, moves: Vector[Move]): Game =
    moves.foldLeft(start) { (game, move) => game.applyMove(move).toOption.get }

  test("all PGN parsers import the same move history and resulting game") {
    val expected = replay(Game.initial, moves)

    NotationParsers.all.foreach { choice =>
      val imported = Pgn.parse(moveTextPgn, choice).toOption.get
      assert(imported.startGame == Game.initial, s"unexpected start game for ${choice.id}")
      assert(imported.moves == moves, s"unexpected move history for ${choice.id}")
      assert(imported.game == expected, s"unexpected final game for ${choice.id}")
    }
  }

  test("PGN export writes movetext and omits FEN for the standard starting position") {
    val exported = Pgn.encode(Game.initial, moves)

    assert(exported.contains("1. e2e4 e7e5 2. g1f3 *"))
    assert(!exported.contains("[FEN "))
    assert(!exported.contains("[SetUp "))
  }

  test("PGN export sets result to 1-0 or 0-1 on checkmate instead of '*'") {
    val whiteMateMoves = Vector(
      Move(Pos(4, 1), Pos(4, 3)),
      Move(Pos(5, 6), Pos(5, 5)),
      Move(Pos(3, 1), Pos(3, 2)),
      Move(Pos(6, 6), Pos(6, 4)),
      Move(Pos(3, 0), Pos(7, 4))
    )
    val blackMateMoves = Vector(
      Move(Pos(5, 1), Pos(5, 2)),
      Move(Pos(4, 6), Pos(4, 4)),
      Move(Pos(6, 1), Pos(6, 3)),
      Move(Pos(3, 7), Pos(7, 3))
    )

    val whiteWin = Pgn.encode(Game.initial, whiteMateMoves)
    val blackWin = Pgn.encode(Game.initial, blackMateMoves)

    assert(whiteWin.contains("""[Result "1-0"]"""))
    assert(whiteWin.trim.endsWith("1-0"))
    assert(blackWin.contains("""[Result "0-1"]"""))
    assert(blackWin.trim.endsWith("0-1"))
  }

  test("PGN export includes FEN only for non-standard starting positions and still round-trips") {
    val customStart = Fen.parse("4k3/8/8/8/8/8/4P3/4K3 w - - 0 1").toOption.get
    val customMoves = Vector(Move(Pos(4, 1), Pos(4, 3)))
    val exported = Pgn.encode(customStart, customMoves)
    val expected = replay(customStart, customMoves)

    assert(exported.contains("""[SetUp "1"]"""))
    assert(exported.contains("""[FEN "4k3/8/8/8/8/8/4P3/4K3 w - - 0 1"]"""))

    NotationParsers.all.foreach { choice =>
      val imported = Pgn.parse(exported, choice).toOption.get
      assert(imported.startGame == customStart, s"unexpected custom start for ${choice.id}")
      assert(imported.moves == customMoves, s"unexpected custom move history for ${choice.id}")
      assert(imported.game == expected, s"unexpected custom final state for ${choice.id}")
    }
  }

  test("PGN parsers reject invalid movetext, invalid result, and invalid setup consistently") {
    val cases = List(
      """[Result "*"]""" -> Right(ImportedPgn(Game.initial, Vector.empty, Game.initial, "*")),
      """[Result "??"]""" -> Left("PGN result must be one of *, 1-0, 0-1, or 1/2-1/2."),
      """1. e4 *""" -> Left("Invalid PGN movetext."),
      """[SetUp "0"]
        |[FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"]
        |
        |*""".stripMargin -> Left("""PGN SetUp tag must be "1" when FEN is present.""")
    )

    cases.foreach { (input, expected) =>
      NotationParsers.all.foreach { choice =>
        assert(Pgn.parse(input, choice) == expected, s"unexpected result for ${choice.id}")
      }
    }
  }

  test("NotationParsers.resolve accepts known ids and rejects unknown ones") {
    assert(NotationParsers.resolve("fastparse").map(_.id) == Right("fastparse"))
    assert(NotationParsers.resolve("Parser Combinators").map(_.id) == Right("combinators"))
    assert(NotationParsers.resolve("wat").left.exists(_.contains("Unknown parser")))
  }

  test("Pgn.parse uses the default parser when no parser choice is supplied") {
    val imported = Pgn.parse(moveTextPgn)
    assert(imported.map(_.moves) == Right(moves))
  }

  test("PGN parsers reject malformed structure and invalid promotion movetext") {
    val malformed = """[Event "Broken" """
    val invalidPromotion = "1. e7e8=K *"

    NotationParsers.all.foreach { choice =>
      assert(Pgn.parse(malformed, choice).isLeft, s"unexpected malformed structure result for ${choice.id}")
    }

    NotationParsers.all.foreach { choice =>
      assert(Pgn.parse(invalidPromotion, choice) == Left("Invalid PGN movetext."), s"unexpected invalid promotion result for ${choice.id}")
    }
  }

  test("PgnSupport.parseMoveToken safely rejects unsupported tokens") {
    assert(PgnSupport.parseMoveToken("invalid") == Left("Unsupported PGN movetext token: invalid"))
  }

  test("PGN import and export handles all pawn promotion roles") {
    val fenStr = "4p1k1/PPPP4/8/8/8/8/pppp4/4P1K1 w"
    val start = Fen.parse(fenStr).toOption.get

    val pgnMoves = Vector(
      Move(Pos(0, 6), Pos(0, 7), Some(PromotionRole.Queen)),
      Move(Pos(0, 1), Pos(0, 0), Some(PromotionRole.Rook)),
      Move(Pos(1, 6), Pos(1, 7), Some(PromotionRole.Bishop)),
      Move(Pos(1, 1), Pos(1, 0), Some(PromotionRole.Knight))
    )

    val exported = Pgn.encode(start, pgnMoves)
    
    assert(exported.contains("a7a8=Q"))
    assert(exported.contains("a2a1=R"))
    assert(exported.contains("b7b8=B"))
    assert(exported.contains("b2b1=N"))

    NotationParsers.all.foreach { choice =>
      val importedResult = Pgn.parse(exported, choice)
      assert(importedResult.isRight, s"unexpected promotion import failure for ${choice.id}: ${importedResult.left.toOption}")
      val imported = importedResult.toOption.get
      assert(imported.moves == pgnMoves, s"unexpected promotion import for ${choice.id}")
    }
  }

  test("FastParse edge cases for PGN whitespace and structural elements") {
    // Tests various combinations of spaces, tabs, carriage returns, newlines, 
    // and edge-case lengths of tags and values to satisfy fastparse macro coverage.
    val pgn = " \t\r\n[A \"b\"]\r\n [C\t\"d\"]  \n1. e2e4 \n\t *"
    val result = FastParsePgnParser.parse(pgn, FastParseFenParser)
    
    assert(result.isRight)
    val imported = result.toOption.get
    // check it got start game implicitly
    assert(imported.startGame == Game.initial)
  }

  test("RegexPgnParser skips leading empty lines correctly (collectTags tailrec)") {
    val pgn = "\n   \n\t\n[Event \"RegexTest\"]\n[Result \"*\"]\n\n1. e2e4 *"
    val imported = RegexPgnParser.parse(pgn, RegexFenParser).toOption.get
    assert(imported.moves.headOption.map(_.from) == Some(Pos(4, 1)))
  }

  test("FastParse branch depth / failure cases for full coverage") {
    // These specific failures force the FastParse macro-generated bytecode 
    // to traverse its "Left/Failure" paths inside the state machine.
    val badInputs = Seq(
      "[", 
      "[A", 
      "[A ", 
      "[A \"", 
      "[A \"b]",
      "[A \"b\"] [",
      " [A \"b\"] \n \n x "
    )
    badInputs.foreach { bad =>
      assert(FastParsePgnParser.parse(bad, FastParseFenParser).isLeft)
    }
  }
