package ch.tichess.services

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.*
import ch.tichess.controller.Controller
import ch.tichess.model.Fen
import ch.tichess.view.{CommandRequest, CommandResponse, JsonSupport}

import scala.concurrent.Await
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object ControllerServer extends JsonSupport:

  def main(args: Array[String]): Unit =
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "TiChessControllerService")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val modelServiceUrl = ServiceConfig.url("MODEL_SERVICE_URL", "http://localhost:8081")
    val modelService: ModelService = new HttpModelService(modelServiceUrl)
    val port = ServiceConfig.port("CONTROLLER_SERVICE_PORT", 8082)

    var appState = Controller.initialState

    val route =
      pathPrefix("api" / "controller") {
        concat(
          post {
            path("update") {
              entity(as[CommandRequest]) { req =>
                onComplete(modelServiceReady(modelService, appState, req.input)) {
                  case Success(res) =>
                    appState = res.state
                    complete(CommandResponse(success = true, res.message, Some(Fen.encode(res.game)), res.quit))
                  case Failure(ex) =>
                    complete(CommandResponse(success = false, Some(ex.getMessage), None, false))
                }
              }
            }
          },
          get {
            path("state") {
              complete(StateResponseBuilder.fromAppState(appState))
            }
          }
        )
      }

    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"Controller service online at http://localhost:$port/")
    Await.result(system.whenTerminated, Duration.Inf)

  private def modelServiceReady(modelService: ModelService, appState: ch.tichess.controller.AppState, input: String)(implicit
      ec: ExecutionContextExecutor
  ) =
    Controller.updateAsync(appState, input, modelService)
