package ch.tichess.controller.persistence.slick

import ch.tichess.controller.persistence.{GameDao, GameRecord}
import _root_.slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

class SlickGameDao(val profile: JdbcProfile)(val db: profile.backend.Database)(implicit ec: ExecutionContext) extends GameDao {
  import profile.api._

  class GamesTable(tag: Tag) extends Table[GameRecord](tag, "games") {
    def id = column[String]("id", O.PrimaryKey)
    def fen = column[String]("fen")
    def pgn = column[String]("pgn")
    def status = column[String]("status")

    def * = (id, fen, pgn, status) <> ((GameRecord.apply _).tupled, GameRecord.unapply)
  }

  val games = TableQuery[GamesTable]

  // Initialize schema if not exists
  def initSchema(): Future[Unit] = db.run(games.schema.createIfNotExists)

  override def save(game: GameRecord): Future[Unit] = 
    db.run(games += game).map(_ => ())

  override def load(id: String): Future[Option[GameRecord]] = 
    db.run(games.filter(_.id === id).result.headOption)

  override def update(game: GameRecord): Future[Unit] = 
    db.run(games.filter(_.id === game.id).update(game)).map(_ => ())

  override def delete(id: String): Future[Unit] = 
    db.run(games.filter(_.id === id).delete).map(_ => ())

  override def listAll(): Future[Seq[GameRecord]] = 
    db.run(games.result)
}
