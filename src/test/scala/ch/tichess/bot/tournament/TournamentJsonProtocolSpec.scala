package ch.tichess.bot.tournament

import org.scalatest.funsuite.AnyFunSuite
import spray.json.*

final class TournamentJsonProtocolSpec extends AnyFunSuite with TournamentJsonProtocol:

  test("parses current tournament-server gameState including clock increment") {
    val event =
      """{
        |  "type": "gameState",
        |  "fen": "8/8/8/8/8/8/8/K6k w - - 0 1",
        |  "moves": "",
        |  "turn": "white",
        |  "clock": {
        |    "whiteTime": 300.0,
        |    "blackTime": 299.5,
        |    "increment": 3
        |  },
        |  "status": "ongoing",
        |  "winner": null
        |}""".stripMargin.parseJson.convertTo[TournamentGameEvent]

    assert(event.`type` == "gameState")
    assert(event.clock.contains(TournamentClock(300.0, 299.5, Some(3.0))))
  }

  test("parses heartbeat events from current NDJSON streams") {
    val tournamentHeartbeat = """{"type":"heartbeat"}""".parseJson.convertTo[TournamentEvent]
    val gameHeartbeat = """{"type":"heartbeat"}""".parseJson.convertTo[TournamentGameEvent]

    assert(tournamentHeartbeat.`type` == "heartbeat")
    assert(gameHeartbeat.`type` == "heartbeat")
  }

  test("parses tournament gameStart participants from player objects") {
    val event =
      """{
        |  "type": "gameStart",
        |  "round": 2,
        |  "gameId": "abc123",
        |  "color": "white",
        |  "white": { "id": "bot-1", "name": "TiChess" },
        |  "black": { "id": "bot-2", "name": "Opponent" }
        |}""".stripMargin.parseJson.convertTo[TournamentEvent]

    assert(event.gameId.contains("abc123"))
    assert(event.white.contains(TournamentPlayer(Some("bot-1"), Some("TiChess"))))
    assert(event.black.contains(TournamentPlayer(Some("bot-2"), Some("Opponent"))))
  }

  test("parses tournament gameStart participants from flat fields") {
    val event =
      """{
        |  "type": "gameStart",
        |  "gameId": "def456",
        |  "color": "black",
        |  "whiteName": "Opponent",
        |  "blackId": "bot-1",
        |  "blackName": "TiChess"
        |}""".stripMargin.parseJson.convertTo[TournamentEvent]

    assert(event.white.contains(TournamentPlayer(None, Some("Opponent"))))
    assert(event.black.contains(TournamentPlayer(Some("bot-1"), Some("TiChess"))))
  }

  test("clock parser remains compatible with payloads without increment") {
    val clock = """{"whiteTime":42.0,"blackTime":41.5}""".parseJson.convertTo[TournamentClock]

    assert(clock == TournamentClock(42.0, 41.5, None))
  }
