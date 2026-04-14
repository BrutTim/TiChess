package ch.tichess.services

import ch.tichess.model.{Game, Move}
import scala.concurrent.Future

trait ModelService:
  def applyMove(game: Game, move: Move): Future[Either[String, Game]]

class LocalModelService extends ModelService:
  override def applyMove(game: Game, move: Move): Future[Either[String, Game]] =
    Future.successful(game.applyMove(move))
