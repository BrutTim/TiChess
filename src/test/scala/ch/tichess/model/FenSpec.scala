package ch.tichess.model

import java.nio.file.Files

import org.scalatest.funsuite.AnyFunSuite

final class FenSpec extends AnyFunSuite:

  private val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"
  private val fullInputError = "FEN must contain exactly placement and side-to-move."

  private def parsers: List[FenParser] = FenParsers.all

  private def assertSameAcrossParsers(input: String, expected: Either[String, Game]): Unit =
    parsers.foreach { parser =>
      assert(Fen.parseWith(parser, input) == expected, s"unexpected result for ${parser.name}")
    }

  test("Fen.encode(Game.initial) matches standard initial FEN") {
    assert(Fen.encode(Game.initial) == initialFen)
  }

  test("all parsers accept initial FEN and allow surrounding whitespace including newlines") {
    assertSameAcrossParsers(initialFen, Right(Game.initial))
    assertSameAcrossParsers(s"  \n$initialFen\t  ", Right(Game.initial))

    val blackFen = initialFen.replace(" w", "\n b")
    val parsedBlack = parsers.map(parser => Fen.parseWith(parser, blackFen).toOption.get)
    assert(parsedBlack.forall(_.sideToMove == Color.Black))
    assert(parsedBlack.forall(_.board.allPieces.size == 32))
  }

  test("all parsers reject missing or extra input instead of leaving rest") {
    assertSameAcrossParsers("8/8/8/8/8/8/8/8", Left(fullInputError))
    assertSameAcrossParsers("8/8/8/8/8/8/8/4K3 w extra", Left(fullInputError))
    assertSameAcrossParsers("not a fen", Left(fullInputError))
  }

  test("all parsers produce the same semantic validation errors") {
    val cases = List(
      "8/8/8/8/8/8/8/4K3 x" -> Left("FEN side-to-move must be 'w' or 'b'."),
      "8/8/8/8/8/8/8 w" -> Left("FEN placement must have 8 ranks separated by '/'."),
      "9/8/8/8/8/8/8/4K2 w" -> Left("FEN digit must be 1..8."),
      "4k3/8/8/8/8/8/8/8 w" -> Left("FEN must contain exactly one white king."),
      "18/4k3/8/8/8/8/8/4K3 w" -> Left("FEN rank has too many squares."),
      "8K/4k3/8/8/8/8/8/4K3 w" -> Left("FEN rank has too many squares."),
      "7/4k3/8/8/8/8/8/4K3 w" -> Left("FEN rank does not cover exactly 8 squares."),
      "8/8/8/8/8/8/8/4.3 w" -> Left("Invalid piece character."),
      "8/8/8/8/8/8/8/4X3 w" -> Left("Invalid piece character."),
      "k6k/8/8/8/8/8/8/4K3 w" -> Left("FEN must contain exactly one black king."))

    cases.foreach { (input, expected) =>
      assertSameAcrossParsers(input, expected)
    }
  }

  test("Fen.encode round-trips a position with mixed empty runs and black to move") {
    val fen = "8/8/8/8/8/3k4/8/KR6 b"
    val games = parsers.map(parser => Fen.parseWith(parser, fen).toOption.get)

    assert(games.distinct.size == 1)
    assert(games.forall(game => Fen.encode(game) == fen))
  }

  test("Fen.parse delegates to the default parser and parseWith exposes individual implementations") {
    val expected = Right(Game.initial)

    assert(Fen.parse(initialFen) == expected)
    assert(Fen.parseWith(Fen.defaultParser, initialFen) == expected)
    assert(parsers.map(parser => Fen.parseWith(parser, initialFen)).distinct == List(expected))
  }

  test("Fen.parseFile loads valid FEN for the default parser and parseFileWith works for all parser variants") {
    val file = Files.createTempFile("tichess-fen-", ".txt")
    Files.writeString(file, s"\n$initialFen\n")

    assert(Fen.parseFile(file.toString) == Right(Game.initial))
    parsers.foreach { parser =>
      assert(Fen.parseFileWith(parser, file.toString) == Right(Game.initial), s"unexpected file parse for ${parser.name}")
    }
  }

  test("Fen.parseFile and parseFileWith report IO and parse failures on the Either error track") {
    val missing = Files.createTempDirectory("tichess-missing-dir").resolve("missing.fen")
    val invalid = Files.createTempFile("tichess-invalid-fen-", ".txt")
    Files.writeString(invalid, "not a fen")

    val ioResult = Fen.parseFile(missing.toString)
    assert(ioResult.isLeft)
    assert(ioResult.left.toOption.exists(_.startsWith("Could not read FEN file:")))

    parsers.foreach { parser =>
      assert(Fen.parseFileWith(parser, invalid.toString) == Left(fullInputError), s"unexpected invalid file result for ${parser.name}")
    }
  }
