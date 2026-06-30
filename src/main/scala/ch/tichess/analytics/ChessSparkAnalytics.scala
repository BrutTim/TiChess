package ch.tichess.analytics

import org.apache.spark.sql.functions.*
import org.apache.spark.sql.streaming.Trigger
import org.apache.spark.sql.types.{LongType, StringType, StructField, StructType}
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.mongodb.scala.*
import org.mongodb.scala.model.{Filters, ReplaceOptions}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}

object ChessSparkAnalytics:

  val eventSchema: StructType =
    StructType(
      Seq(
        StructField("eventId", StringType, nullable = false),
        StructField("gameId", StringType, nullable = false),
        StructField("eventType", StringType, nullable = false),
        StructField("command", StringType, nullable = false),
        StructField("winner", StringType, nullable = true),
        StructField("result", StringType, nullable = true),
        StructField("moveCount", LongType, nullable = false),
        StructField("timestamp", LongType, nullable = false),
        StructField("fen", StringType, nullable = false)
      )
    )

  def playerOutcomes(events: DataFrame): DataFrame =
    events
      .filter(col("eventType") === "GameFinished")
      .select(
        explode(
          array(
            struct(
              lit("White").as("player"),
              when(col("winner") === "White", lit("win"))
                .when(col("winner").isNull, lit("draw"))
                .otherwise(lit("loss"))
                .as("outcome")
            ),
            struct(
              lit("Black").as("player"),
              when(col("winner") === "Black", lit("win"))
                .when(col("winner").isNull, lit("draw"))
                .otherwise(lit("loss"))
                .as("outcome")
            )
          )
        ).as("entry")
      )
      .select("entry.*")

  def aggregate(events: DataFrame): DataFrame =
    playerOutcomes(events)
      .groupBy("player")
      .agg(
        count(lit(1)).as("games"),
        count(when(col("outcome") === "win", true)).as("victories"),
        count(when(col("outcome") === "draw", true)).as("draws"),
        count(when(col("outcome") === "loss", true)).as("losses"),
        sum(
          when(col("outcome") === "win", 3)
            .when(col("outcome") === "draw", 1)
            .otherwise(0)
        ).as("score")
      )
      .orderBy(col("score").desc, col("victories").desc, col("player"))

  def readEventsFromFile(spark: SparkSession, path: String): DataFrame =
    spark.read.schema(eventSchema).json(path)

  def readEventsFromKafka(
      spark: SparkSession,
      bootstrapServers: String,
      topic: String
  ): DataFrame =
    spark.readStream
      .format("kafka")
      .option("kafka.bootstrap.servers", bootstrapServers)
      .option("subscribe", topic)
      .option("startingOffsets", "earliest")
      .load()
      .selectExpr("CAST(value AS STRING) AS json")
      .select(from_json(col("json"), eventSchema).as("event"))
      .select("event.*")
      .filter(col("eventId").isNotNull)

  private def statisticsDocument(row: Row, updatedAt: Long): Document =
    Document(
      "_id" -> row.getAs[String]("player"),
      "games" -> row.getAs[Long]("games"),
      "victories" -> row.getAs[Long]("victories"),
      "draws" -> row.getAs[Long]("draws"),
      "losses" -> row.getAs[Long]("losses"),
      "score" -> row.getAs[Long]("score"),
      "updatedAt" -> updatedAt
    )

  def writeStatistics(batch: DataFrame, mongoUri: String): Unit =
    given ExecutionContext = ExecutionContext.global
    val rows = batch.collect().toSeq
    if rows.nonEmpty then
      val client = MongoClient(mongoUri)
      try
        val collection = client
          .getDatabase("tichess")
          .getCollection("player_statistics")
        val updatedAt = System.currentTimeMillis()
        val writes = rows.map { row =>
          val document = statisticsDocument(row, updatedAt)
          collection
            .replaceOne(
              Filters.equal("_id", row.getAs[String]("player")),
              document,
              ReplaceOptions().upsert(true)
            )
            .toFuture()
        }
        Await.result(Future.sequence(writes), 30.seconds)
      finally client.close()

  def sparkSession(appName: String): SparkSession =
    SparkSession
      .builder()
      .appName(appName)
      .master(sys.env.getOrElse("SPARK_MASTER", "local[*]"))
      .config("spark.ui.enabled", "false")
      .getOrCreate()

  def main(args: Array[String]): Unit =
    args.toList match
      case "file" :: path :: Nil =>
        val spark = sparkSession("TiChessSparkFileAnalytics")
        try
          aggregate(readEventsFromFile(spark, path)).show(truncate = false)
        finally spark.stop()

      case "kafka" :: bootstrapServers :: topic :: Nil =>
        val spark = sparkSession("TiChessSparkKafkaAnalytics")
        val mongoUri = sys.env.getOrElse("MONGO_URI", "mongodb://localhost:27017")
        val checkpoint =
          sys.env.getOrElse(
            "SPARK_CHECKPOINT_LOCATION",
            "/tmp/tichess-spark-checkpoint"
          )

        val persistBatch: (DataFrame, Long) => Unit = (batch, _) =>
          batch.show(truncate = false)
          writeStatistics(batch, mongoUri)

        aggregate(readEventsFromKafka(spark, bootstrapServers, topic))
          .writeStream
          .outputMode("complete")
          .option("checkpointLocation", checkpoint)
          .trigger(Trigger.ProcessingTime("5 seconds"))
          .foreachBatch(persistBatch)
          .start()
          .awaitTermination()

      case _ =>
        System.err.println(
          """Usage:
            |  sbt "runMain ch.tichess.analytics.ChessSparkAnalytics file examples/spark-game-events.jsonl"
            |  sbt "runMain ch.tichess.analytics.ChessSparkAnalytics kafka localhost:9092 tichess.game-events"
            |""".stripMargin
        )
        sys.exit(1)
