package ch.tichess.controller

import ch.tichess.controller.persistence.ChallengeRecord
import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

final class ControllerCoverageSpec extends AnyFunSuite:

  given ExecutionContext = ExecutionContext.global

  test("Command.parse covers command aliases and parser spacing branches") {
    assert(Command.parse("exit") == Right(Command.Quit))
    assert(Command.parse("h") == Right(Command.Help))
    assert(Command.parse("ablehnen") == Right(Command.DrawDecline))
    assert(Command.parse("aufgeben") == Right(Command.Resign))
    assert(Command.parse("neu") == Right(Command.NewGame))
    assert(Command.parse("restart") == Right(Command.NewGame))
    assert(Command.parse("challenge") == Left("Expected a challenge command like: challenge random."))
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

  test("Controller.updateAsync default challenge hooks are usable") {
    val helped = Await.result(
      Controller.updateAsync(Controller.initialState, "help", new ch.tichess.services.LocalModelService()),
      5.seconds
    )
    val defaultLoad = Await.result(
      Controller.updateAsync(Controller.initialState, "challenge load absent", new ch.tichess.services.LocalModelService()),
      5.seconds
    )
    val defaultRandom = Await.result(
      Controller.updateAsync(Controller.initialState, "challenge random", new ch.tichess.services.LocalModelService()),
      5.seconds
    )

    assert(helped.message.exists(_.contains("Zug eingeben")))
    assert(defaultLoad.message.contains("Challenge nicht gefunden: absent"))
    assert(defaultRandom.message.contains("Keine Challenge verfuegbar."))
  }

  test("Controller covers challenge loading errors and empty solved state") {
    val emptyChallenge = ChallengeRecord("empty", "Empty", Fen.encode(Game.initial), "")
    val nonMoveChallenge = emptyChallenge.copy(id = "bad-command", moves = "help")
    val badFenChallenge = emptyChallenge.copy(id = "bad-fen", fen = "not a fen", moves = "e2 e4")

    val missingLoad = Await.result(
      Controller.updateAsync(
        Controller.initialState,
        "challenge load missing",
        new ch.tichess.services.LocalModelService(),
        challengeLookup = _ => Future.successful(None)
      ),
      5.seconds
    )
    assert(missingLoad.message.contains("Challenge nicht gefunden: missing"))

    val emptyLoad = Await.result(
      Controller.updateAsync(
        Controller.initialState,
        "challenge load empty",
        new ch.tichess.services.LocalModelService(),
        challengeLookup = _ => Future.successful(Some(emptyChallenge))
      ),
      5.seconds
    )
    assert(emptyLoad.message.exists(_.contains("Challenge has no solution moves.")))

    val nonMoveLoad = Await.result(
      Controller.updateAsync(
        Controller.initialState,
        "challenge load bad-command",
        new ch.tichess.services.LocalModelService(),
        challengeLookup = _ => Future.successful(Some(nonMoveChallenge))
      ),
      5.seconds
    )
    assert(nonMoveLoad.message.exists(_.contains("Expected a move in challenge solution")))

    val missingRandom = Await.result(
      Controller.updateAsync(
        Controller.initialState,
        "challenge random",
        new ch.tichess.services.LocalModelService(),
        randomChallenge = () => Future.successful(None)
      ),
      5.seconds
    )
    assert(missingRandom.message.contains("Keine Challenge verfuegbar."))

    val badRandom = Await.result(
      Controller.updateAsync(
        Controller.initialState,
        "challenge random",
        new ch.tichess.services.LocalModelService(),
        randomChallenge = () => Future.successful(Some(badFenChallenge))
      ),
      5.seconds
    )
    assert(badRandom.message.exists(_.contains("Challenge konnte nicht geladen werden:")))

    val alreadySolvedState = Controller.initialState.copy(
      challengeMode = Some(ChallengeState("done", "Done", Vector.empty))
    )
    val alreadySolved = Await.result(
      Controller.updateAsync(alreadySolvedState, "e2 e4", new ch.tichess.services.LocalModelService()),
      5.seconds
    )
    assert(alreadySolved.state.challengeCompleted)
    assert(alreadySolved.message.contains("Challenge geloest!"))
  }

  test("Controller covers challenge move failures and reply completion") {
    val illegalPlayerState = Controller.initialState.copy(
      challengeMode = Some(ChallengeState("illegal-player", "Illegal player", Vector(Move(Pos(4, 1), Pos(4, 4)))))
    )
    val illegalPlayer = Await.result(
      Controller.updateAsync(illegalPlayerState, "e2 e5", new ch.tichess.services.LocalModelService()),
      5.seconds
    )
    assert(illegalPlayer.message.exists(_.contains("Illegal")))

    val illegalReplyState = Controller.initialState.copy(
      challengeMode = Some(
        ChallengeState(
          "illegal-reply",
          "Illegal reply",
          Vector(Move(Pos(4, 1), Pos(4, 3)), Move(Pos(4, 6), Pos(4, 2)))
        )
      )
    )
    val illegalReply = Await.result(
      Controller.updateAsync(illegalReplyState, "e2 e4", new ch.tichess.services.LocalModelService()),
      5.seconds
    )
    assert(illegalReply.message.exists(_.contains("Antwortzug konnte nicht ausgefuehrt werden")))
    assert(illegalReply.state.challengeMode.exists(_.remainingMoves.size == 1))

    val solvedAfterReplyState = Controller.initialState.copy(
      challengeMode = Some(
        ChallengeState(
          "reply-solve",
          "Reply solve",
          Vector(Move(Pos(4, 1), Pos(4, 3)), Move(Pos(4, 6), Pos(4, 4)))
        )
      )
    )
    val solvedAfterReply = Await.result(
      Controller.updateAsync(solvedAfterReplyState, "e2 e4", new ch.tichess.services.LocalModelService()),
      5.seconds
    )
    assert(solvedAfterReply.state.challengeCompleted)
    assert(solvedAfterReply.message.contains("Challenge geloest!"))
  }
