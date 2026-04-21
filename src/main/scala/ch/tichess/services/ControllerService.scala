package ch.tichess.services

import ch.tichess.controller.{AppState, Controller, UpdateResult}
import scala.concurrent.{ExecutionContext, Future}

trait ControllerService:
  def update(state: AppState, input: String): Future[UpdateResult]

class LocalControllerService(modelService: ModelService)(implicit ec: ExecutionContext) extends ControllerService:
  override def update(state: AppState, input: String): Future[UpdateResult] =
    Controller.updateAsync(state, input, modelService)
