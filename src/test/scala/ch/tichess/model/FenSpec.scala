package ch.tichess.model

import java.nio.file.Files

import org.scalatest.funsuite.AnyFunSuite

final class FenSpec extends AnyFunSuite:

  private val initialFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"

  test("Fen.encode(Game.initial) matches standard initial FEN (minimal subset)") {
    assert(Fen.encode(Game.initial) == initialFen)
  }

  test("Fen.parse accepts initial FEN and sets side-to-move") {
    assert(Fen.parse(initialFen) == Right(Game.initial))

    val blackFen = initialFen.replace(" w", " b")
    val parsedBlack = Fen.parse(blackFen).toOption.get
    assert(parsedBlack.sideToMove == Color.Black)
    assert(parsedBlack.board.allPieces.size == 32)
  }

  test("Fen.parse rejects invalid side-to-move") {
    assert(Fen.parse("8/8/8/8/8/8/8/4K3 x") == Left("FEN side-to-move must be 'w' or 'b'."))
  }

  test("Fen.parsePlacement rejects wrong number of ranks") {
    assert(Fen.parse("8/8/8/8/8/8/8/8") == Left("FEN must contain at least placement and side-to-move."))
    assert(Fen.parse("8/8/8/8/8/8/8 w 0 1") == Left("FEN placement must have 8 ranks separated by '/'."))
  }

  test("Fen.parsePlacement rejects invalid digits and missing kings") {
    assert(Fen.parse("9/8/8/8/8/8/8/4K2 w") == Left("FEN digit must be 1..8."))
    assert(Fen.parse("4k3/8/8/8/8/8/8/8 w") == Left("FEN must contain exactly one white king."))
  }

  test("Fen.encode also supports side-to-move black") {
    val blackFen = initialFen.replace(" w", " b")
    val parsedBlack = Fen.parse(blackFen).toOption.get
    assert(Fen.encode(parsedBlack) == blackFen)
  }

  test("Fen.parsePlacement rejects ranks with too many squares / too few squares") {
    // rank8: "18" => 1 empty + 8 empty => 9 squares -> overflow
    assert(Fen.parse("18/4k3/8/8/8/8/8/4K3 w") == Left("FEN rank has too many squares."))

    // rank8: "8K" => 8 empties then a piece => overflow (file > 7)
    assert(Fen.parse("8K/4k3/8/8/8/8/8/4K3 w") == Left("FEN rank has too many squares."))

    // rank8: "7" => only 7 squares covered
    assert(Fen.parse("7/4k3/8/8/8/8/8/4K3 w") == Left("FEN rank does not cover exactly 8 squares."))
  }

  test("Fen.parsePlacement rejects invalid piece characters") {
    // '.' is neither upper nor lower => charToPiece color branch error
    assert(Fen.parse("8/8/8/8/8/8/8/4.3 w") == Left("Invalid piece character."))

    // 'X' is upper => color ok, but kind invalid
    assert(Fen.parse("8/8/8/8/8/8/8/4X3 w") == Left("Invalid piece character."))
  }

  test("Fen.parsePlacement rejects multiple kings for a side") {
    // two black kings, one white king
    assert(Fen.parse("k6k/8/8/8/8/8/8/4K3 w") == Left("FEN must contain exactly one black king."))
  }

  test("Fen.encode covers emptyCount>0 and emptyCount==0 branches") {
    val fen = "8/8/8/8/8/3k4/8/KR6 w"
    val game = Fen.parse(fen).toOption.get
    assert(Fen.encode(game) == fen)
  }

  test("Fen.parseFile loads a valid FEN via Try-based file handling") {
    val file = Files.createTempFile("tichess-fen-", ".txt")
    Files.writeString(file, initialFen)

    assert(Fen.parseFile(file.toString) == Right(Game.initial))
  }

  test("Fen.parseFile reports IO errors through the error track") {
    val missing = Files.createTempDirectory("tichess-missing-dir").resolve("missing.fen")
    val result = Fen.parseFile(missing.toString)

    assert(result.isLeft)
    assert(result.left.toOption.exists(_.startsWith("Could not read FEN file:")))
  }

  test("Fen.parseFile keeps parse failures on the same two-track pipeline") {
    val file = Files.createTempFile("tichess-invalid-fen-", ".txt")
    Files.writeString(file, "not a fen")

    assert(Fen.parseFile(file.toString) == Left("FEN side-to-move must be 'w' or 'b'."))
  }
