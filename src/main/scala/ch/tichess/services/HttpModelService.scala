package ch.tichess.services

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.unmarshalling.Unmarshal
import ch.tichess.model.{Fen, Game, Move, PromotionRole}
import ch.tichess.view.{JsonSupport, ModelResponse, MoveRequest}

import scala.concurrent.{ExecutionContext, Future}

final class HttpModelService(baseUrl: String)(implicit system: ActorSystem[?], ec: ExecutionContext)
    extends ModelService
    with JsonSupport:

  override def applyMove(game: Game, move: Move): Future[Either[String, Game]] =
    val request = MoveRequest(Fen.encode(game), encodeMove(move))

    for
      entity <- Marshal(request).to[akka.http.scaladsl.model.RequestEntity]
      response <- Http().singleRequest(
        akka.http.scaladsl.model.HttpRequest(
          method = akka.http.scaladsl.model.HttpMethods.POST,
          uri = s"$baseUrl/api/model/applyMove",
          entity = entity
        )
      )
      payload <- Unmarshal(response.entity).to[ModelResponse]
    yield
      if payload.success then
        payload.fen match
          case Some(nextFen) => Fen.parse(nextFen)
          case None          => Left("Model service returned success without a FEN.")
      else Left(payload.error.getOrElse("Model service rejected the move."))

  private def encodeMove(move: Move): String =
    val promotionSuffix = move.promotion.map(role => s" ${promotionChar(role)}").getOrElse("")
    s"${move.from.toAlgebraic} ${move.to.toAlgebraic}$promotionSuffix"

  private def promotionChar(role: PromotionRole): String = role match
    case PromotionRole.Queen  => "q"
    case PromotionRole.Rook   => "r"
    case PromotionRole.Bishop => "b"
    case PromotionRole.Knight => "n"
