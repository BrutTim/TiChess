package ch.tichess.view

import org.scalatest.funsuite.AnyFunSuite
import ch.tichess.model.Game
import ch.tichess.model.Color
import ch.tichess.model.PieceType
import ch.tichess.model.Board
import ch.tichess.model.Piece
import ch.tichess.model.Pos

class GuiViewStateSpec extends AnyFunSuite {

  test("GuiViewState isGameOver evaluates correctly") {
    val initial = GuiViewState.initial
    assert(!initial.isGameOver)

    val drawAgreed = initial.copy(drawAgreed = true)
    assert(drawAgreed.isGameOver)

    val resigned = initial.copy(resignedBy = Some(Color.White))
    assert(resigned.isGameOver)

    // Checkmate state
    val mateBoard = Board.empty.copy(
      pieces = Map(
        Pos(0, 0) -> Piece(Color.White, PieceType.King),
        Pos(1, 1) -> Piece(Color.Black, PieceType.Queen),
        Pos(2, 2) -> Piece(Color.Black, PieceType.King)
      )
    )
    val checkmateGame = Game(mateBoard, Color.White)
    assert(checkmateGame.isCheckmate)
    
    val checkmateState = initial.copy(game = checkmateGame)
    assert(checkmateState.isGameOver)
  }

  test("GuiViewState statusText combinations") {
    val initial = GuiViewState.initial
    assert(initial.statusText == "White to move")
    
    val checkBoard = Board.empty.copy(
      pieces = Map(
        Pos(0, 0) -> Piece(Color.White, PieceType.King),
        Pos(0, 7) -> Piece(Color.Black, PieceType.Rook),
        Pos(7, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val checkGame = Game(checkBoard, Color.White)
    val checkState = initial.copy(game = checkGame)
    assert(checkState.statusText == "White to move | Schach")

    val drawState = initial.copy(game = initial.game.copy(halfMoveClock = 100))
    assert(drawState.statusText == "Remis - 50-Züge-Regel")

    val remisAgreed = initial.copy(drawAgreed = true)
    assert(remisAgreed.statusText == "Remis - Einigung")

    val drawOffer = initial.copy(drawOfferedBy = Some(Color.White))
    assert(drawOffer.statusText == "White to move | White bietet Remis an")

    // Both check and draw offer
    val checkDrawState = checkState.copy(drawOfferedBy = Some(Color.Black))
    assert(checkDrawState.statusText == "White to move | Schach | Black bietet Remis an")

    val resignedState = initial.copy(resignedBy = Some(Color.Black))
    assert(resignedState.statusText == "Black hat aufgegeben. White gewinnt!")
  }

  test("GuiViewState captured and material advantage calculation") {
    val initial = GuiViewState.initial
    assert(initial.materialAdvantage == 0)
    assert(initial.capturedByWhite.isEmpty)
    assert(initial.capturedByBlack.isEmpty)

    val gameBoardMissingWhiteQueen = Board.initial.copy(
      pieces = Board.initial.pieces - Pos(3, 0)
    )
    val stateMissingWhiteQueen = initial.copy(game = Game(gameBoardMissingWhiteQueen, Color.Black))

    assert(stateMissingWhiteQueen.capturedByBlack == List(PieceType.Queen))
    assert(stateMissingWhiteQueen.materialAdvantage == -9)

    val gameBoardMissingBlackPawn = Board.initial.copy(
      pieces = Board.initial.pieces - Pos(4, 6)
    )
    val stateMissingBlackPawn = initial.copy(game = Game(gameBoardMissingBlackPawn, Color.White))
    assert(stateMissingBlackPawn.capturedByWhite == List(PieceType.Pawn))
    assert(stateMissingBlackPawn.materialAdvantage == 1)
  }
}
