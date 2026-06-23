package ch.tichess.analytics

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

  test("aggregate counts victories and score by winner") {
    val events =
      spark.createDataFrame(
        Seq(
          Row(1L, "new", true, true, "Neues Spiel gestartet.", "start"),
          Row(2L, "resign", true, true, "Black gibt auf. White gewinnt!", "fen"),
          Row(3L, "new", true, true, "Neues Spiel gestartet.", "start"),
          Row(4L, "resign", true, true, "White gibt auf. Black gewinnt!", "fen"),
          Row(5L, "bad command", false, false, "Invalid command.", null)
        ).asJava,
        ChessSparkAnalytics.eventSchema
      )

    val rows = ChessSparkAnalytics
      .aggregate(events)
      .select("player", "victories", "score")
      .collect()
      .map(row => row.getString(0) -> (row.getLong(1), row.getLong(2)))
      .toMap

    assert(rows("White") == (1L, 3L))
    assert(rows("Black") == (1L, 3L))
    assert(rows("No winner") == (0L, -1L))
  }
