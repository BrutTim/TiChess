package ch.tichess.analytics

import ch.tichess.controller.{AppState, Controller}
import ch.tichess.model.{Color, Fen}
import org.apache.spark.sql.{Row, SparkSession}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import scala.jdk.CollectionConverters.*

class ChessSparkAnalyticsSpec extends AnyFunSuite with BeforeAndAfterAll:

  private val spark =
    SparkSession
      .builder()
      .appName("ChessSparkAnalyticsSpec")
      .master("local[2]")
      .config("spark.ui.enabled", "false")
      .getOrCreate()

  override def afterAll(): Unit =
    try spark.stop()
    finally super.afterAll()

  test("aggregate creates a White and Black leaderboard from finished games") {
    val events =
      spark.createDataFrame(
        Seq(
          Row("1", "game-1", "GameStarted", "new", null, null, 0L, 1L, "start"),
          Row("2", "game-1", "GameFinished", "resign", "White", "resignation", 2L, 2L, "fen"),
          Row("3", "game-2", "GameFinished", "resign", "Black", "resignation", 0L, 3L, "fen"),
          Row("4", "game-3", "GameFinished", "accept", null, "draw", 20L, 4L, "fen")
        ).asJava,
        ChessSparkAnalytics.eventSchema
      )

    val rows = ChessSparkAnalytics
      .aggregate(events)
      .collect()
      .map { row =>
        row.getAs[String]("player") ->
          (
            row.getAs[Long]("games"),
            row.getAs[Long]("victories"),
            row.getAs[Long]("draws"),
            row.getAs[Long]("losses"),
            row.getAs[Long]("score")
          )
      }
      .toMap

    assert(rows("White") == (3L, 1L, 1L, 1L, 4L))
    assert(rows("Black") == (3L, 1L, 1L, 1L, 4L))
  }

  test("GameEventFactory emits a structured finish event for resignation") {
    val before = Controller.initialState
    val after = before.copy(resignedBy = Some(Color.White))

    val event = GameEventFactory
      .create(
        gameId = "game-1",
        input = "resign",
        before = before,
        after = after,
        message = Some("White gibt auf. Black gewinnt!"),
        timestamp = 123L
      )
      .get

    assert(event.eventType == "GameFinished")
    assert(event.winner.contains("Black"))
    assert(event.result.contains("resignation"))
    assert(event.timestamp == 123L)
  }

  test("GameEventFactory ignores commands that do not change gameplay") {
    assert(
      GameEventFactory
        .create(
          gameId = "game-1",
          input = "fen export",
          before = Controller.initialState,
          after = Controller.initialState,
          message = Some("fen")
        )
        .isEmpty
    )
  }

  test("GameEventFactory covers start, moves, bot moves and result sources") {
    val before = Controller.initialState
    val afterMove = Controller.update(before, "e2 e4").state

    val startEvent = GameEventFactory.create("game-2", "new", before, before, None).get
    val moveEvent = GameEventFactory.create("game-2", "e2 e4", before, afterMove, None).get
    val botEvent = GameEventFactory.create("game-2", "bot move", before, afterMove, None).get

    assert(startEvent.eventType == "GameStarted")
    assert(moveEvent.eventType == "MovePlayed")
    assert(moveEvent.moveCount == 1L)
    assert(botEvent.eventType == "MovePlayed")

    val checkmateState = AppState(Fen.parse("7k/6Q1/6K1/8/8/8/8/8 b - - 0 1").toOption.get)
    val checkmateEvent = GameEventFactory
      .create("game-2", "g7 g8", before, checkmateState, Some("Checkmate"), timestamp = 99L)
      .get
    assert(checkmateEvent.eventType == "GameFinished")
    assert(checkmateEvent.winner.contains("White"))
    assert(checkmateEvent.result.contains("checkmate"))

    val drawState = before.copy(drawAgreed = true)
    val drawEvent = GameEventFactory.create("game-2", "accept", before, drawState, Some("Remis")).get
    assert(drawEvent.winner.isEmpty)
    assert(drawEvent.result.contains("draw"))

    val whiteImport = GameEventFactory.create("game-2", "pgn import *", before, before, Some("White wins by PGN")).get
    val blackImport = GameEventFactory.create("game-2", "pgn import *", before, before, Some("Black wins by PGN")).get
    val remisImport = GameEventFactory.create("game-2", "pgn import *", before, before, Some("Remis (laut PGN)")).get

    assert(whiteImport.winner.contains("White"))
    assert(blackImport.winner.contains("Black"))
    assert(remisImport.winner.isEmpty)
    assert(remisImport.result.contains("draw"))
    assert(GameEventFactory.create("game-2", "accept", drawState, drawState, Some("Remis")).isEmpty)
  }
