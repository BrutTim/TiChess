package ch.tichess.controller.persistence.mongo

import ch.tichess.controller.persistence.{ChallengeDao, ChallengeRecord}
import org.mongodb.scala._
import org.mongodb.scala.model.Filters._

import scala.concurrent.{ExecutionContext, Future}

class MongoChallengeDao(collection: MongoCollection[Document])(implicit ec: ExecutionContext) extends ChallengeDao {

  private def toDocument(challenge: ChallengeRecord): Document =
    Document(
      "_id" -> challenge.id,
      "name" -> challenge.name,
      "fen" -> challenge.fen,
      "moves" -> challenge.moves
    )

  private def fromDocument(doc: Document): ChallengeRecord =
    ChallengeRecord(
      id = doc.getString("_id"),
      name = doc.getString("name"),
      fen = doc.getString("fen"),
      moves = doc.getString("moves")
    )

  override def save(challenge: ChallengeRecord): Future[Unit] =
    collection.insertOne(toDocument(challenge)).toFuture().map(_ => ())

  override def load(id: String): Future[Option[ChallengeRecord]] =
    collection.find(equal("_id", id)).first().headOption().map(_.map(fromDocument))

  override def update(challenge: ChallengeRecord): Future[Unit] =
    collection.replaceOne(equal("_id", challenge.id), toDocument(challenge)).toFuture().map(_ => ())

  override def delete(id: String): Future[Unit] =
    collection.deleteOne(equal("_id", id)).toFuture().map(_ => ())

  override def listAll(): Future[Seq[ChallengeRecord]] =
    collection.find().toFuture().map(_.map(fromDocument))
}
