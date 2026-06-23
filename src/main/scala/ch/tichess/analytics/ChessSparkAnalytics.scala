package ch.tichess.analytics

import org.apache.spark.sql.functions.*
import org.apache.spark.sql.streaming.Trigger
import org.apache.spark.sql.types.{BooleanType, LongType, StringType, StructField, StructType}
import org.apache.spark.sql.{DataFrame, SparkSession}

object ChessSparkAnalytics:

  val eventSchema: StructType =
    StructType(
      Seq(
        StructField("line", LongType, nullable = false),
        StructField("input", StringType, nullable = false),
        StructField("accepted", BooleanType, nullable = false),
        StructField("success", BooleanType, nullable = false),
        StructField("message", StringType, nullable = true),
        StructField("fen", StringType, nullable = true)
      )
    )

  def withMetrics(events: DataFrame): DataFrame =
    events
      .withColumn("messageText", coalesce(col("message"), lit("")))
      .withColumn(
        "winner",
        when(
          col("success") &&
            (lower(col("messageText")).contains("white wins") ||
              lower(col("messageText")).contains("white gewinnt")),
          lit("White")
        ).when(
          col("success") &&
            (lower(col("messageText")).contains("black wins") ||
              lower(col("messageText")).contains("black gewinnt")),
          lit("Black")
        ).otherwise(lit(null))
      )
      .withColumn(
        "draw",
        col("success") &&
          (lower(col("messageText")).contains("draw") ||
            lower(col("messageText")).startsWith("spiel durch remis") ||
            lower(col("messageText")).contains("remis (laut pgn)"))
      )
      .withColumn(
        "score",
        when(col("winner").isNotNull, lit(3))
          .when(col("draw"), lit(1))
          .when(col("success"), lit(0))
          .otherwise(lit(-1))
      )

  def aggregate(events: DataFrame): DataFrame =
    withMetrics(events)
      .groupBy(col("winner").as("player"))
      .agg(
        count(when(col("success"), true)).as("successfulEvents"),
        count(when(!col("success"), true)).as("failedEvents"),
        count(when(col("winner").isNotNull, true)).as("victories"),
        count(when(col("draw"), true)).as("draws"),
        sum(col("score")).as("score")
      )
      .na
      .fill("No winner", Seq("player"))
      .orderBy(col("score").desc, col("victories").desc, col("player"))

  def readEventsFromFile(spark: SparkSession, path: String): DataFrame =
    spark.read.schema(eventSchema).json(path)

  def readEventsFromKafka(spark: SparkSession, bootstrapServers: String, topic: String): DataFrame =
    spark.readStream
      .format("kafka")
      .option("kafka.bootstrap.servers", bootstrapServers)
      .option("subscribe", topic)
      .option("startingOffsets", "earliest")
      .load()
      .selectExpr("CAST(value AS STRING) AS json")
      .select(from_json(col("json"), eventSchema).as("event"))
      .select("event.*")

  def sparkSession(appName: String): SparkSession =
    SparkSession
      .builder()
      .appName(appName)
      .master(sys.env.getOrElse("SPARK_MASTER", "local[*]"))
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
        aggregate(readEventsFromKafka(spark, bootstrapServers, topic))
          .writeStream
          .outputMode("complete")
          .format("console")
          .option("truncate", "false")
          .trigger(Trigger.ProcessingTime("5 seconds"))
          .start()
          .awaitTermination()

      case _ =>
        System.err.println(
          """Usage:
            |  sbt "runMain ch.tichess.analytics.ChessSparkAnalytics file examples/spark-game-events.jsonl"
            |  sbt "runMain ch.tichess.analytics.ChessSparkAnalytics kafka localhost:9092 tichess.events"
            |""".stripMargin
        )
        sys.exit(1)
