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

  test("PGN export includes FEN only for non-standard starting positions and still round-trips") {
    val customStart = Fen.parse("4k3/8/8/8/8/8/4P3/4K3 w").toOption.get
    val customMoves = Vector(Move(Pos(4, 1), Pos(4, 3)))
    val exported = Pgn.encode(customStart, customMoves)
    val expected = replay(customStart, customMoves)

    assert(exported.contains("""[SetUp "1"]"""))
    assert(exported.contains("""[FEN "4k3/8/8/8/8/8/4P3/4K3 w"]"""))

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
