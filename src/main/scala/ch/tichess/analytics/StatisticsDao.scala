package ch.tichess.analytics

import org.mongodb.scala.*
import org.mongodb.scala.model.Filters.equal

import scala.concurrent.{ExecutionContext, Future}

trait StatisticsDao:
  def list(): Future[Seq[PlayerStatistics]]

object EmptyStatisticsDao extends StatisticsDao:
  override def list(): Future[Seq[PlayerStatistics]] = Future.successful(Seq.empty)

final class MongoStatisticsDao(collection: MongoCollection[Document])(using ec: ExecutionContext)
    extends StatisticsDao:

  private def longValue(document: Document, key: String): Long =
    document.get(key).map(_.asNumber().longValue()).getOrElse(0L)

  private def fromDocument(document: Document): PlayerStatistics =
    PlayerStatistics(
      player = document.getString("_id"),
      games = longValue(document, "games"),
      victories = longValue(document, "victories"),
      draws = longValue(document, "draws"),
      losses = longValue(document, "losses"),
      score = longValue(document, "score"),
      updatedAt = longValue(document, "updatedAt")
    )

  override def list(): Future[Seq[PlayerStatistics]] =
    collection
      .find()
      .sort(Document("score" -> -1, "victories" -> -1, "_id" -> 1))
      .toFuture()
      .map(_.map(fromDocument))
