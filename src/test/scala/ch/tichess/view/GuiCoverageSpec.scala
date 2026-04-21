package ch.tichess.view

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class GuiCoverageSpec extends AnyFunSuite:

  private def parseState(fen: String): GuiViewState =
    GuiViewState(Fen.parse(fen).fold(err => fail(err), identity))

  test("GuiViewState covers remaining status and material branches") {
    val fiftyMove = GuiViewState(
      Game(
        Board.empty.copy(
          pieces = Map(
            Pos(0, 0) -> Piece(Color.White, PieceType.King),
            Pos(7, 7) -> Piece(Color.Black, PieceType.King)
          )
        ),
        Color.White,
        halfMoveClock = 100
      )
    )
    assert(fiftyMove.statusText == "Remis - 50-Züge-Regel")

    val capturedKindsState = GuiViewState(
      game = Game(
        Board.empty.copy(
          pieces = Map(
            Pos(0, 0) -> Piece(Color.White, PieceType.King),
            Pos(1, 0) -> Piece(Color.White, PieceType.Queen),
            Pos(7, 7) -> Piece(Color.Black, PieceType.King)
          )
        ),
        Color.White
      ),
      startGame = Game.initial
    )

    val whiteCaptured = capturedKindsState.capturedByWhite
    val blackCaptured = capturedKindsState.capturedByBlack
    assert(whiteCaptured.contains(PieceType.Pawn))
    assert(whiteCaptured.contains(PieceType.Knight))
    assert(whiteCaptured.contains(PieceType.Bishop))
    assert(whiteCaptured.contains(PieceType.Rook))
    assert(blackCaptured.contains(PieceType.Pawn))
    assert(blackCaptured.contains(PieceType.Bishop))
    assert(blackCaptured.contains(PieceType.Rook))
    assert(blackCaptured.contains(PieceType.Knight))
  }

  test("GuiViewAdapter covers game-over draw handling, resign, and reset helpers") {
    val gameOver = parseState("k7/1Q6/2K5/8/8/8/8/8 b - - 0 1")
    assert(GuiViewAdapter.drawOffer(gameOver) == gameOver)
    assert(GuiViewAdapter.resign(gameOver) == gameOver)

    val initial = new GuiViewAdapter().initialState
    val withSelection = initial.copy(selectedPos = Some(Pos(4, 1)), legalTargetSquares = Set(Pos(4, 2)))
    val resigned = GuiViewAdapter.resign(withSelection)
    assert(resigned.resignedBy.contains(Color.White))
    assert(resigned.selectedPos.isEmpty)
    assert(resigned.legalTargetSquares.isEmpty)

    val fresh = GuiViewAdapter.newGame()
    assert(fresh.game == Game.initial)
    assert(fresh.infoMessage.contains("Neues Spiel gestartet."))

    val noPendingAccept = GuiViewAdapter.drawAccept(initial)
    assert(noPendingAccept.infoMessage.contains("Kein Remis-Angebot vorhanden."))
    val noPendingDecline = GuiViewAdapter.drawDecline(initial)
    assert(noPendingDecline.infoMessage.contains("Kein Remis-Angebot vorhanden."))
  }

  test("GuiViewAdapter covers PGN result branches and export with agreed draw") {
    val initial = new GuiViewAdapter().initialState

    val drawImported = GuiViewAdapter.setPgn(initial, "1. e2e4 1/2-1/2")
    assert(drawImported.drawAgreed)
    assert(drawImported.infoMessage.contains("Remis (laut PGN)."))

    val whiteImported = GuiViewAdapter.setPgn(initial, "1. e2e4 1-0")
    assert(whiteImported.drawAgreed)
    assert(whiteImported.infoMessage.contains("White wins (laut PGN)."))

    val blackImported = GuiViewAdapter.setPgn(initial, "1. e2e4 0-1")
    assert(blackImported.drawAgreed)
    assert(blackImported.infoMessage.contains("Black wins (laut PGN)."))

    val exported = GuiViewAdapter.exportPgn(drawImported)
    assert(exported.notationText.contains("1/2-1/2"))
  }

  test("GuiViewAdapter SAN rendering covers castling, pawn capture, and black-first log entries") {
    val castleGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 0) -> Piece(Color.White, PieceType.King),
          Pos(7, 0) -> Piece(Color.White, PieceType.Rook),
          Pos(0, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    )
    val castleEntries = GuiViewAdapter.buildMoveEntries(castleGame, Vector(Move(Pos(4, 0), Pos(6, 0))))
    assert(castleEntries.last.contains("O-O"))

    val pawnCaptureGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(0, 0) -> Piece(Color.White, PieceType.King),
          Pos(4, 4) -> Piece(Color.White, PieceType.Pawn),
          Pos(3, 5) -> Piece(Color.Black, PieceType.Knight),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    )
    val pawnCaptureEntries = GuiViewAdapter.buildMoveEntries(pawnCaptureGame, Vector(Move(Pos(4, 4), Pos(3, 5))))
    assert(pawnCaptureEntries.last.contains("exd6"))

    val blackFirstGame = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(4, 7) -> Piece(Color.Black, PieceType.King),
          Pos(0, 7) -> Piece(Color.Black, PieceType.Rook),
          Pos(4, 0) -> Piece(Color.White, PieceType.King)
        )
      ),
      Color.Black
    )
    val blackFirstEntries = GuiViewAdapter.buildMoveEntries(blackFirstGame, Vector(Move(Pos(4, 7), Pos(2, 7))))
    assert(blackFirstEntries.last.contains("..."))
    assert(blackFirstEntries.last.contains("O-O-O"))
  }

  test("ConsoleView covers remaining capture-line formatting branches") {
    val materialLead = Game(
      Board.empty.copy(
        pieces = Map(
          Pos(0, 0) -> Piece(Color.White, PieceType.King),
          Pos(1, 0) -> Piece(Color.White, PieceType.Queen),
          Pos(7, 7) -> Piece(Color.Black, PieceType.King)
        )
      ),
      Color.White
    )

    val rendered = ConsoleView.render(materialLead)
    assert(rendered.contains("White captured:"))
    assert(rendered.contains("Black captured:"))
    assert(rendered.contains("+"))
  }
