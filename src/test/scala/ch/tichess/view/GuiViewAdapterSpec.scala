package ch.tichess.view

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class GuiViewAdapterSpec extends AnyFunSuite:
  private def parseState(fen: String): GuiViewState =
    GuiViewState(Fen.parse(fen).fold(err => fail(err), identity))

  test("selecting an own piece marks all legal targets") {
    val adapter = new GuiViewAdapter()
    val initial = adapter.initialState
    val from = Pos(4, 1) // e2

    val updated = GuiViewAdapter.handleSquareClick(initial, from)

    assert(updated.selectedPos.contains(from))
    assert(updated.legalTargetSquares == Set(Pos(4, 2), Pos(4, 3)))
  }

  test("successful move updates side-to-move and appends white/black move log entries") {
    val adapter = new GuiViewAdapter()
    val initial = adapter.initialState

    val afterWhiteSelect = GuiViewAdapter.handleSquareClick(initial, Pos(4, 1)) // e2
    val afterWhiteMove = GuiViewAdapter.handleSquareClick(afterWhiteSelect, Pos(4, 3)) // e4

    val whiteSan = "e4"
    val expectedWhiteEntry = f"${1}%-4d $whiteSan"

    assert(afterWhiteMove.game.sideToMove == Color.Black)
    assert(afterWhiteMove.moveEntries.last == expectedWhiteEntry)
    assert(afterWhiteMove.moveHistory == Vector(Move(Pos(4, 1), Pos(4, 3))))

    val afterBlackSelect = GuiViewAdapter.handleSquareClick(afterWhiteMove, Pos(4, 6)) // e7
    val afterBlackMove = GuiViewAdapter.handleSquareClick(afterBlackSelect, Pos(4, 4)) // e5

    val blackSan = "e5"
    val paddedWhiteEntry = f"${expectedWhiteEntry}%-18s"
    val expectedCombinedEntry = s"$paddedWhiteEntry $blackSan"
    
    assert(afterBlackMove.game.sideToMove == Color.White)
    assert(afterBlackMove.moveEntries.last == expectedCombinedEntry)
    assert(afterBlackMove.moveHistory == Vector(Move(Pos(4, 1), Pos(4, 3)), Move(Pos(4, 6), Pos(4, 4))))
  }

  test("setFen sets game state, clears move log and updates status on checkmate") {
    val adapter = new GuiViewAdapter()
    val initial = adapter.initialState

    val afterMove =
      GuiViewAdapter.handleSquareClick(
        GuiViewAdapter.handleSquareClick(initial, Pos(4, 1)),
        Pos(4, 3)
      )
    assert(afterMove.moveEntries.nonEmpty)

    val updated = GuiViewAdapter.setFen(afterMove, "k7/1Q6/2K5/8/8/8/8/8 b - - 0 1")

    assert(updated.game.isCheckmate)
    assert(updated.isGameOver)
    assert(updated.statusText.contains("Schachmatt"))
    assert(updated.moveEntries.isEmpty)
    assert(updated.infoMessage.contains("Position gesetzt mit fastparse."))
  }

  test("status text reports normal turns, check, and black to move") {
    val initial = new GuiViewAdapter().initialState
    assert(initial.statusText == "White to move")

    val afterWhiteMove =
      GuiViewAdapter.handleSquareClick(
        GuiViewAdapter.handleSquareClick(initial, Pos(4, 1)),
        Pos(4, 3)
      )
    assert(afterWhiteMove.statusText == "Black to move")

    val inCheck = parseState("4k3/8/8/8/8/8/4r3/4K3 w - - 0 1")
    assert(inCheck.statusText == "White to move | Schach")
  }

  test("handleSquareClick covers invalid click, reselection, deselection, and game-over ignore") {
    val initial = new GuiViewAdapter().initialState

    val afterEmptyClick = GuiViewAdapter.handleSquareClick(initial, Pos(0, 2))
    assert(afterEmptyClick == initial)

    val selected = GuiViewAdapter.handleSquareClick(initial, Pos(4, 1))
    val unchangedWhileSelected = GuiViewAdapter.handleSquareClick(selected, Pos(0, 2))
    assert(unchangedWhileSelected == selected)

    val deselected = GuiViewAdapter.handleSquareClick(selected, Pos(4, 1))
    assert(deselected.selectedPos.isEmpty)
    assert(deselected.legalTargetSquares.isEmpty)

    val reselected = GuiViewAdapter.handleSquareClick(selected, Pos(3, 1))
    assert(reselected.selectedPos.contains(Pos(3, 1)))
    assert(reselected.legalTargetSquares == Set(Pos(3, 2), Pos(3, 3)))

    val gameOver = parseState("k7/1Q6/2K5/8/8/8/8/8 b - - 0 1")
    assert(GuiViewAdapter.handleSquareClick(gameOver, Pos(0, 7)) == gameOver)
  }

  test("setFen reports parse errors and leaves the current state otherwise intact") {
    val initial = new GuiViewAdapter().initialState
    val updated = GuiViewAdapter.setFen(initial, "not a fen")

    assert(updated.game == initial.game)
    assert(updated.selectedPos == initial.selectedPos)
    assert(updated.legalTargetSquares == initial.legalTargetSquares)
    assert(updated.moveEntries == initial.moveEntries)
    assert(updated.infoMessage.nonEmpty)
  }

  test("GUI parser selection, FEN export, and PGN import/export use the selected parser") {
    val initial = new GuiViewAdapter().initialState

    val parserChanged = GuiViewAdapter.setParser(initial, "regex")
    assert(parserChanged.selectedParserId == "regex")
    assert(parserChanged.infoMessage.contains("Parser gesetzt: regex."))

    val fenExported = GuiViewAdapter.exportFen(parserChanged)
    assert(fenExported.notationText == Fen.encode(initial.game))
    assert(fenExported.infoMessage.contains("FEN exportiert."))

    val pgnExported = GuiViewAdapter.exportPgn(parserChanged)
    assert(pgnExported.notationText.contains("[Result"))
    assert(pgnExported.notationText.trim.endsWith("*"))
    assert(pgnExported.infoMessage.contains("PGN exportiert."))

    val imported = GuiViewAdapter.setPgn(
      parserChanged,
      "1. f2f3 e7e5 2. g2g4 *"
    )
    assert(imported.selectedParserId == "regex")
    assert(imported.moveEntries.nonEmpty)
    assert(imported.moveHistory.size == 3)
    assert(imported.infoMessage.contains("PGN importiert mit regex."))
  }

  test("GUI reports invalid parser ids and invalid PGN without changing the game") {
    val initial = new GuiViewAdapter().initialState

    val badParser = GuiViewAdapter.setParser(initial, "wat")
    assert(badParser.game == initial.game)
    assert(badParser.infoMessage.exists(_.contains("Unknown parser")))

    val badPgn = GuiViewAdapter.setPgn(initial, "1. e4 *")
    assert(badPgn.game == initial.game)
    assert(badPgn.infoMessage.contains("Invalid PGN movetext."))
  }

  test("choosePromotion keeps state unchanged when no promotion is pending") {
    val initial = new GuiViewAdapter().initialState
    assert(GuiViewAdapter.choosePromotion(initial, PromotionRole.Queen) == initial)
  }

  test("attemptMove surfaces illegal target errors when state and target disagree") {
    val initial = new GuiViewAdapter().initialState
    val inconsistentState =
      initial.copy(
        selectedPos = Some(Pos(4, 1)),
        legalTargetSquares = Set(Pos(4, 4))
      )

    val updated = GuiViewAdapter.handleSquareClick(inconsistentState, Pos(4, 4))

    assert(updated.infoMessage.exists(_.contains("Illegal")))
    assert(updated.game == initial.game)
    assert(updated.selectedPos.contains(Pos(4, 1)))
  }

  test("successful moves can announce check and checkmate") {
    val checkingState = parseState("4k3/8/8/8/8/8/4R3/4K3 w - - 0 1")
    val afterCheck =
      GuiViewAdapter.handleSquareClick(
        GuiViewAdapter.handleSquareClick(checkingState, Pos(4, 1)),
        Pos(4, 6)
      )

    assert(afterCheck.infoMessage.contains("Schach"))
    assert(afterCheck.game.sideToMove == Color.Black)

    val matingState = parseState("k7/8/1QK5/8/8/8/8/8 w - - 0 1")
    val afterMate =
      GuiViewAdapter.handleSquareClick(
        GuiViewAdapter.handleSquareClick(matingState, Pos(1, 5)),
        Pos(1, 6)
      )

    assert(afterMate.isGameOver)
    assert(afterMate.infoMessage.contains("Schachmatt - White gewinnt"))
  }

  test("promotion in GUI requires a choice and applies the selected role") {
    val state = parseState("7k/4P3/8/8/8/8/8/K7 w - - 0 1")

    val pending =
      GuiViewAdapter.handleSquareClick(
        GuiViewAdapter.handleSquareClick(state, Pos(4, 6)),
        Pos(4, 7)
      )

    assert(pending.pendingPromotion.contains(PendingPromotion(Pos(4, 6), Pos(4, 7), Color.White)))
    assert(pending.infoMessage.contains("Promotion wählen: Dame, Turm, Läufer oder Springer."))
    assert(GuiViewAdapter.handleSquareClick(pending, Pos(0, 0)) == pending)

    val promoted = GuiViewAdapter.choosePromotion(pending, PromotionRole.Knight)
    assert(promoted.pendingPromotion.isEmpty)
    assert(promoted.game.board.pieceAt(Pos(4, 7)).contains(Piece(Color.White, PieceType.Knight)))
    assert(promoted.moveEntries.last.endsWith("e8=S"))

    val cancelled = GuiViewAdapter.cancelPromotion(pending)
    assert(cancelled.pendingPromotion.isEmpty)
    assert(cancelled.infoMessage.isEmpty)
  }

  test("promotion choice covers queen rook bishop and rejects invalid roles") {
    val pending = parseState("7k/4P3/8/8/8/8/8/K7 w - - 0 1").copy(
      pendingPromotion = Some(PendingPromotion(Pos(4, 6), Pos(4, 7), Color.White)),
      infoMessage = Some("Promotion wählen: Dame, Turm, Läufer oder Springer.")
    )

    val queenPromoted = GuiViewAdapter.choosePromotion(pending, PromotionRole.Queen)
    assert(queenPromoted.game.board.pieceAt(Pos(4, 7)).contains(Piece(Color.White, PieceType.Queen)))
    assert(queenPromoted.moveEntries.last.endsWith("e8=D+"))

    val rookPromoted = GuiViewAdapter.choosePromotion(pending, PromotionRole.Rook)
    assert(rookPromoted.game.board.pieceAt(Pos(4, 7)).contains(Piece(Color.White, PieceType.Rook)))
    assert(rookPromoted.moveEntries.last.endsWith("e8=T+"))

    val bishopPromoted = GuiViewAdapter.choosePromotion(pending, PromotionRole.Bishop)
    assert(bishopPromoted.game.board.pieceAt(Pos(4, 7)).contains(Piece(Color.White, PieceType.Bishop)))
    assert(bishopPromoted.moveEntries.last.endsWith("e8=L"))
  }

  test("buildMoveEntries silently ignores invalid moves") {
    val game = Game.initial
    val badMove = Move(Pos(4, 1), Pos(4, 5)) // e2 to e6, illegal for pawn
    val entries = GuiViewAdapter.buildMoveEntries(game, Vector(badMove))
    assert(entries.isEmpty)
  }
