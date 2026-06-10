package ch.tichess.streaming

import akka.actor.CoordinatedShutdown
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.*
import ch.tichess.services.{ControllerHttpClient, ServiceConfig}
import ch.tichess.view.JsonSupport

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.util.{Failure, Success}

object StreamServer extends JsonSupport:

  def main(args: Array[String]): Unit =
    given system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "TiChessStreamService")
    given executionContext: ExecutionContextExecutor = system.executionContext

    val port = ServiceConfig.port("STREAM_SERVICE_PORT", 8083)
    val controllerUrl = ServiceConfig.url("CONTROLLER_SERVICE_URL", "http://localhost:8082")
    val kafkaBootstrap = sys.env.getOrElse("KAFKA_BOOTSTRAP_SERVERS", "localhost:9092")
    val commandsTopic = sys.env.getOrElse("KAFKA_COMMANDS_TOPIC", "tichess.commands")
    val eventsTopic = sys.env.getOrElse("KAFKA_EVENTS_TOPIC", "tichess.events")
    val groupId = sys.env.getOrElse("KAFKA_CONSUMER_GROUP", "tichess-stream-service")

    val controllerClient = new ControllerHttpClient(controllerUrl)
    val kafkaProducer = new KafkaCommandProducer(kafkaBootstrap, commandsTopic)
    val kafkaBridge = new KafkaCommandBridge(
      kafkaBootstrap,
      commandsTopic,
      eventsTopic,
      groupId,
      controllerClient.update
    )
    val bridgeControl = kafkaBridge.start()

    CoordinatedShutdown(system).addTask(
      CoordinatedShutdown.PhaseServiceStop,
      "drain-kafka-command-bridge"
    ) { () =>
      bridgeControl.drainAndShutdown()
    }

    val route =
      concat(
        path("health") {
          get {
            complete("ok")
          }
        },
        pathPrefix("api" / "stream") {
          post {
            path("commands") {
              entity(as[String]) { body =>
                onComplete(ChessCommandStream.runText(body, controllerClient.update)) {
                  case Success(results) => complete(results.toList)
                  case Failure(error)   => failWith(error)
                }
              }
            }
          }
        },
        pathPrefix("api" / "kafka") {
          post {
            path("commands") {
              entity(as[String]) { body =>
                onComplete(kafkaProducer.publishText(body)) {
                  case Success(response) => complete(response)
                  case Failure(error)    => failWith(error)
                }
              }
            }
          }
        }
      )

    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"Stream service online at http://localhost:$port/")
    println(s"Kafka bridge: $commandsTopic -> controller -> $eventsTopic")
    Await.result(system.whenTerminated, Duration.Inf)
