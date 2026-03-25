package ch.tichess.view

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class GuiViewAdapterSpec extends AnyFunSuite:

  test("selecting an own piece marks all legal targets") {
    val adapter = new GuiViewAdapter()
    val from = Pos(4, 1) // e2

    adapter.handleSquareClick(from)

    assert(adapter.selectedPos.contains(from))
    assert(adapter.legalTargetSquares == Set(Pos(4, 2), Pos(4, 3)))
  }

  test("successful move updates side-to-move and appends white/black move log entries") {
    val adapter = new GuiViewAdapter()

    adapter.handleSquareClick(Pos(4, 1)) // e2
    adapter.handleSquareClick(Pos(4, 3)) // e4

    assert(adapter.game.sideToMove == Color.Black)
    assert(adapter.moveEntries.last == "1. W e2-e4")

    adapter.handleSquareClick(Pos(4, 6)) // e7
    adapter.handleSquareClick(Pos(4, 4)) // e5

    assert(adapter.game.sideToMove == Color.White)
    assert(adapter.moveEntries.last == "1. B e7-e5")
  }

  test("setFen sets game state, clears move log and updates status on checkmate") {
    val adapter = new GuiViewAdapter()

    adapter.handleSquareClick(Pos(4, 1))
    adapter.handleSquareClick(Pos(4, 3))
    assert(adapter.moveEntries.nonEmpty)

    adapter.setFen("k7/1Q6/2K5/8/8/8/8/8 b")

    assert(adapter.game.isCheckmate)
    assert(adapter.isGameOver)
    assert(adapter.statusText.contains("Schachmatt"))
    assert(adapter.moveEntries.isEmpty)
    assert(adapter.infoMessage.contains("Position gesetzt."))
  }
