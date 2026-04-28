package ch.tichess.controller.persistence

import scala.concurrent.Future

case class GameRecord(id: String, fen: String, pgn: String, status: String)

trait GameDao {
  def save(game: GameRecord): Future[Unit]
  def load(id: String): Future[Option[GameRecord]]
  def update(game: GameRecord): Future[Unit]
  def delete(id: String): Future[Unit]
  def listAll(): Future[Seq[GameRecord]]
}
