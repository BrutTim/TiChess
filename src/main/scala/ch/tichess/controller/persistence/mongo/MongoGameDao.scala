package ch.tichess.controller.persistence.mongo

import ch.tichess.controller.persistence.{GameDao, GameRecord}
import org.mongodb.scala._
import org.mongodb.scala.model.Filters._

import scala.concurrent.{ExecutionContext, Future}

class MongoGameDao(collection: MongoCollection[Document])(implicit ec: ExecutionContext) extends GameDao {

  private def toDocument(game: GameRecord): Document =
    Document("_id" -> game.id, "fen" -> game.fen, "pgn" -> game.pgn, "status" -> game.status)

  private def fromDocument(doc: Document): GameRecord =
    GameRecord(
      id = doc.getString("_id"),
      fen = doc.getString("fen"),
      pgn = doc.getString("pgn"),
      status = doc.getString("status")
    )

  override def save(game: GameRecord): Future[Unit] =
    collection.insertOne(toDocument(game)).toFuture().map(_ => ())

  override def load(id: String): Future[Option[GameRecord]] =
    collection.find(equal("_id", id)).first().headOption().map(_.map(fromDocument))

  override def update(game: GameRecord): Future[Unit] =
    collection.replaceOne(equal("_id", game.id), toDocument(game)).toFuture().map(_ => ())

  override def delete(id: String): Future[Unit] =
    collection.deleteOne(equal("_id", id)).toFuture().map(_ => ())

  override def listAll(): Future[Seq[GameRecord]] =
    collection.find().toFuture().map(_.map(fromDocument))
}
