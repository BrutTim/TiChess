package ch.tichess.services

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{HttpMethods, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import ch.tichess.analytics.PlayerStatistics
import ch.tichess.view.{CommandRequest, CommandResponse, JsonSupport, StateResponse}

import scala.concurrent.{ExecutionContext, Future}

final class ControllerHttpClient(baseUrl: String)(implicit system: ActorSystem[?], ec: ExecutionContext)
    extends JsonSupport:

  def update(input: String): Future[CommandResponse] =
    val request = CommandRequest(input)
    for
      entity <- Marshal(request).to[akka.http.scaladsl.model.RequestEntity]
      response <- Http().singleRequest(
        HttpRequest(
          method = HttpMethods.POST,
          uri = s"$baseUrl/api/controller/update",
          entity = entity
        )
      )
      payload <- Unmarshal(response.entity).to[CommandResponse]
    yield payload

  def fetchState(): Future[StateResponse] =
    for
      response <- Http().singleRequest(
        HttpRequest(
          method = HttpMethods.GET,
          uri = s"$baseUrl/api/controller/state"
        )
      )
      payload <- Unmarshal(response.entity).to[StateResponse]
    yield payload

  def fetchStatistics(): Future[List[PlayerStatistics]] =
    for
      response <- Http().singleRequest(
        HttpRequest(
          method = HttpMethods.GET,
          uri = s"$baseUrl/api/controller/statistics"
        )
      )
      payload <- Unmarshal(response.entity).to[List[PlayerStatistics]]
    yield payload
