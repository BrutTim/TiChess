package ch.tichess.controller

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class ControllerCoverageSpec extends AnyFunSuite:

  test("Command.parse covers command aliases and parser spacing branches") {
    assert(Command.parse("exit") == Right(Command.Quit))
    assert(Command.parse("h") == Right(Command.Help))
    assert(Command.parse("ablehnen") == Right(Command.DrawDecline))
    assert(Command.parse("aufgeben") == Right(Command.Resign))
    assert(Command.parse("neu") == Right(Command.NewGame))
    assert(Command.parse("restart") == Right(Command.NewGame))
    assert(Command.parse("parser    fastparse") == Right(Command.SetParserCmd("fastparse")))
    assert(Command.parse("pgn import") == Left("Position must have length 2 (e.g. e2)."))
  }

  test("Controller.update covers resign and new game branches") {
    val stateAfterMove = Controller.update(Controller.initialState, "e2 e4").state

    val resigned = Controller.update(stateAfterMove, "resign")
    assert(resigned.quit)
    assert(resigned.state.resignedBy.contains(Color.Black))
    assert(resigned.message.contains("Black gibt auf. White gewinnt!"))

    val restarted = Controller.update(stateAfterMove, "new")
    assert(!restarted.quit)
    assert(restarted.state == Controller.initialState)
    assert(restarted.message.contains("Neues Spiel gestartet."))
  }

  test("Controller.update covers stalemate FEN import branch") {
    val stalemateFen = "k7/2Q5/2K5/8/8/8/8/8 b - - 0 1"
    val res = Controller.update(Controller.initialState, s"fen $stalemateFen")

    assert(res.quit)
    assert(res.game.isDraw)
    assert(res.message.contains("Draw (Stalemate)."))
  }

  test("Controller.update covers PGN result branches without mate or stalemate") {
    val draw = Controller.update(Controller.initialState, "pgn import 1. e2e4 1/2-1/2")
    assert(draw.quit)
    assert(draw.message.contains("Remis (laut PGN)."))

    val whiteWins = Controller.update(Controller.initialState, "pgn import 1. e2e4 1-0")
    assert(whiteWins.quit)
    assert(whiteWins.message.contains("White wins (laut PGN)."))

    val blackWins = Controller.update(Controller.initialState, "pgn import 1. e2e4 0-1")
    assert(blackWins.quit)
    assert(blackWins.message.contains("Black wins (laut PGN)."))
  }

  test("Controller.update covers stalemate after a normal move") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(2, 5) -> Piece(Color.White, PieceType.King),
        Pos(1, 5) -> Piece(Color.White, PieceType.Queen),
        Pos(0, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val game = Game(board, Color.White)

    val res = Controller.update(game, "b6 c7")
    assert(res.quit)
    assert(res.message.contains("Draw (Stalemate)."))
  }
