package ch.tichess.view

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class ViewSpec extends AnyFunSuite:

  test("ConsoleView.renderBoard prints ranks/files and pieces") {
    val b = Board.empty.copy(
      pieces = Map(
        Pos(0, 0) -> Piece(Color.White, PieceType.King),
        Pos(7, 7) -> Piece(Color.Black, PieceType.Queen),
        Pos(1, 0) -> Piece(Color.White, PieceType.Queen),
        Pos(2, 0) -> Piece(Color.White, PieceType.Rook),
        Pos(3, 0) -> Piece(Color.White, PieceType.Bishop),
        Pos(4, 0) -> Piece(Color.White, PieceType.Knight),
        Pos(5, 1) -> Piece(Color.White, PieceType.Pawn),
        Pos(6, 7) -> Piece(Color.Black, PieceType.King),
        Pos(5, 7) -> Piece(Color.Black, PieceType.Rook),
        Pos(4, 7) -> Piece(Color.Black, PieceType.Bishop),
        Pos(3, 7) -> Piece(Color.Black, PieceType.Knight),
        Pos(2, 6) -> Piece(Color.Black, PieceType.Pawn)
      )
    )
    val s = ConsoleView.renderBoard(b)
    assert(s.contains("8"))
    assert(s.contains("1"))
    assert(s.contains("a b c d e f g h"))
    assert(s.contains("K"))
    assert(s.contains("q"))
    assert(s.contains("Q"))
    assert(s.contains("R"))
    assert(s.contains("B"))
    assert(s.contains("N"))
    assert(s.contains("P"))
    assert(s.contains("k"))
    assert(s.contains("r"))
    assert(s.contains("b"))
    assert(s.contains("n"))
    assert(s.contains("p"))
  }

  test("ConsoleView.render includes optional message and turn label") {
    val g = Game(Board.empty, Color.Black)
    val s1 = ConsoleView.render(g, None)
    assert(s1.startsWith("Black to move"))

    val s2 = ConsoleView.render(g, Some("Oops"))
    assert(s2.startsWith("Oops\nBlack to move"))
  }

  test("ConsoleView.render uses default message parameter") {
    val g = Game(Board.empty, Color.White)
    val s = ConsoleView.render(g)
    assert(s.startsWith("White to move"))
  }

