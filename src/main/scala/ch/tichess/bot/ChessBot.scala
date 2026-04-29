package ch.tichess.bot

import ch.tichess.controller.AppState
import ch.tichess.model.Move

import scala.concurrent.Future

trait ChessBot:
  def name: String
  def chooseMove(state: AppState, remainingTimeMs: Option[Long] = None): Future[Either[String, Move]]

