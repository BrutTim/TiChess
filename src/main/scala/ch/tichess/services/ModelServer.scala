package ch.tichess.services

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.*
import ch.tichess.controller.Command
import ch.tichess.model.Fen
import ch.tichess.view.{JsonSupport, ModelResponse, MoveRequest}

import scala.concurrent.Await
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object ModelServer extends JsonSupport:

  def main(args: Array[String]): Unit =
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "TiChessModelService")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val modelService = new LocalModelService()
    val port = ServiceConfig.port("MODEL_SERVICE_PORT", 8081)

    val route =
      pathPrefix("api" / "model") {
        post {
          path("applyMove") {
            entity(as[MoveRequest]) { req =>
              Fen.parse(req.fen) match
                case Left(err) =>
                  complete(ModelResponse(success = false, None, Some(s"Invalid FEN: $err")))
                case Right(game) =>
                  Command.parse(req.algebraicMove) match
                    case Right(Command.MoveCmd(move)) =>
                      onComplete(modelService.applyMove(game, move)) {
                        case Success(Right(nextGame)) =>
                          complete(ModelResponse(success = true, Some(Fen.encode(nextGame)), None))
                        case Success(Left(err)) =>
                          complete(ModelResponse(success = false, None, Some(err)))
                        case Failure(ex) =>
                          complete(ModelResponse(success = false, None, Some(ex.getMessage)))
                      }
                    case _ =>
                      complete(ModelResponse(success = false, None, Some("Invalid algebraic move.")))
            }
          }
        }
      }

    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"Model service online at http://localhost:$port/")
    Await.result(system.whenTerminated, Duration.Inf)
