package ch.tichess.services

import ch.tichess.controller.{AppState, ChallengeState, Controller}
import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class StateResponseBuilderSpec extends AnyFunSuite:

  test("fromAppState exposes legal moves, parser, last move, bot and challenge metadata") {
    val moved = Game.initial.applyMove(Move(Pos(4, 1), Pos(4, 3))).toOption.get
    val challengeMove = Move(Pos(4, 6), Pos(4, 4))
    val state = AppState(
      game = moved,
      startGame = Game.initial,
      moveHistory = Vector(Move(Pos(4, 1), Pos(4, 3))),
      challengeMode = Some(ChallengeState("id", "Name", Vector(challengeMove))),
      activeBot = Some(Color.Black)
    )

    val response = StateResponseBuilder.fromAppState(state)

    assert(response.statusText == "Challenge aktiv.")
    assert(!response.isGameOver)
    assert(response.lastMoveFrom.contains("e2"))
    assert(response.lastMoveTo.contains("e4"))
    assert(response.currentParser == "fastparse")
    assert(response.availableParsers.contains("regex"))
    assert(response.challengeHintFrom.contains("e7"))
    assert(response.challengeSideToMove.contains("Schwarz"))
    assert(response.activeBot.contains("b"))
    assert(response.legalMoves.nonEmpty)
    assert(response.moveList.nonEmpty)
  }

  test("fromAppState formats captured material, draw offers and completed challenges") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(3, 0) -> Piece(Color.White, PieceType.Queen),
        Pos(4, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val state = AppState(
      game = Game(board, Color.White),
      drawOfferedBy = Some(Color.Black),
      challengeCompleted = true
    )

    val response = StateResponseBuilder.fromAppState(state)

    assert(response.statusText == "Challenge geloest.")
    assert(response.isGameOver)
    assert(response.drawOffered)
    assert(response.whiteCaptured.contains("♟"))
    assert(response.whiteCaptured.contains("+"))
    assert(response.blackCaptured.contains("♟"))
    assert(response.challengeHintFrom.isEmpty)
    assert(response.challengeSideToMove.isEmpty)
    assert(response.activeBot.isEmpty)
  }

  test("fromAppState covers normal status, white challenge turn and white bot labels") {
    val challengeMove = Move(Pos(1, 0), Pos(2, 2))
    val state = AppState(
      game = Game.initial,
      challengeMode = Some(ChallengeState("knight", "Knight", Vector(challengeMove))),
      activeBot = Some(Color.White)
    )

    val challenge = StateResponseBuilder.fromAppState(state)
    assert(challenge.challengeSideToMove.contains("Weiss"))
    assert(challenge.activeBot.contains("w"))

    val normal = StateResponseBuilder.fromAppState(Controller.initialState)
    assert(normal.statusText.contains("White"))
    assert(normal.whiteCaptured.isEmpty)
    assert(normal.blackCaptured.isEmpty)

    assert(StateResponseBuilder.captureChar(PieceType.King) == "♚")
  }
