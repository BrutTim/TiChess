package ch.tichess.controller.persistence.slick

import ch.tichess.controller.persistence.{ChallengeDao, ChallengeRecord}
import _root_.slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class SlickChallengeDao(val profile: JdbcProfile)(val db: profile.backend.Database)(implicit ec: ExecutionContext)
    extends ChallengeDao {
  import profile.api._

  class ChallengesTable(tag: Tag) extends Table[ChallengeRecord](tag, "challenges") {
    def id = column[String]("id", O.PrimaryKey)
    def name = column[String]("name")
    def fen = column[String]("fen")
    def moves = column[String]("moves")

    def * = (id, name, fen, moves) <> ((ChallengeRecord.apply _).tupled, ChallengeRecord.unapply)
  }

  val challenges = TableQuery[ChallengesTable]

  def initSchema(): Future[Unit] =
    db.run(challenges.schema.createIfNotExists)

  override def save(challenge: ChallengeRecord): Future[Unit] =
    db.run(challenges += challenge).map(_ => ())

  override def load(id: String): Future[Option[ChallengeRecord]] =
    db.run(challenges.filter(_.id === id).result.headOption)

  override def update(challenge: ChallengeRecord): Future[Unit] =
    db.run(challenges.filter(_.id === challenge.id).update(challenge)).map(_ => ())

  override def delete(id: String): Future[Unit] =
    db.run(challenges.filter(_.id === id).delete).map(_ => ())

  override def listAll(): Future[Seq[ChallengeRecord]] =
    db.run(challenges.result)
}
