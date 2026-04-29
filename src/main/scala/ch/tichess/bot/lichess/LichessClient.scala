package ch.tichess.bot.lichess

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import akka.stream.scaladsl.{Framing, Source}
import akka.util.ByteString
import spray.json.*

import scala.concurrent.{ExecutionContext, Future}

// --- JSON Models ---
case class ChallengeVariant(key: String, name: String)
case class Challenge(id: String, variant: ChallengeVariant)
case class GameStartInfo(gameId: String, color: Option[String] = None)
case class LichessEvent(`type`: String, challenge: Option[Challenge], game: Option[GameStartInfo])

case class GameState(moves: String, status: String, wtime: Option[Long] = None, btime: Option[Long] = None)
case class GameFull(id: String, state: GameState)
case class GameEvent(`type`: String, moves: Option[String] = None, status: Option[String] = None, state: Option[GameState] = None, wtime: Option[Long] = None, btime: Option[Long] = None, initialFen: Option[String] = None)

trait LichessJsonProtocol extends DefaultJsonProtocol:
  // Using jsonFormatN ignores extra fields by default in spray-json.
  implicit val variantFormat: RootJsonFormat[ChallengeVariant] = jsonFormat2(ChallengeVariant.apply)
  implicit val challengeFormat: RootJsonFormat[Challenge] = jsonFormat2(Challenge.apply)
  implicit val gameStartFormat: RootJsonFormat[GameStartInfo] = jsonFormat2(GameStartInfo.apply)
  implicit val eventFormat: RootJsonFormat[LichessEvent] = jsonFormat3(LichessEvent.apply)

  implicit val gameStateFormat: RootJsonFormat[GameState] = jsonFormat4(GameState.apply)
  implicit val gameFullFormat: RootJsonFormat[GameFull] = jsonFormat2(GameFull.apply)
  implicit val gameEventFormat: RootJsonFormat[GameEvent] = jsonFormat7(GameEvent.apply)

class LichessClient(token: String)(implicit system: ActorSystem[?], ec: ExecutionContext) extends LichessJsonProtocol:
  private val authHeader = Authorization(OAuth2BearerToken(token))

  /**
   * Stream of incoming events (challenges, game starts).
   */
  def streamEvents(): Source[LichessEvent, ?] =
    val request = HttpRequest(
      uri = "https://lichess.org/api/stream/event",
      headers = List(authHeader)
    )
    
    Source.futureSource(
      Http().singleRequest(request).map { response =>
        if response.status.isSuccess() then
          response.entity.dataBytes
            .via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 65536, allowTruncation = true))
            .map(_.utf8String.trim)
            .filter(_.nonEmpty)
            .map(_.parseJson.convertTo[LichessEvent])
        else
          response.discardEntityBytes()
          Source.failed(new RuntimeException(s"Failed to connect to event stream: ${response.status}"))
      }
    )

  /**
   * Accepts a challenge.
   */
  def acceptChallenge(challengeId: String): Future[Unit] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"https://lichess.org/api/challenge/$challengeId/accept",
      headers = List(authHeader)
    )
    Http().singleRequest(request).map { res =>
      res.discardEntityBytes()
      if !res.status.isSuccess() then throw new RuntimeException(s"Failed to accept challenge: ${res.status}")
    }

  /**
   * Stream of events for a specific game.
   */
  def streamGameEvents(gameId: String): Source[GameEvent, ?] =
    val request = HttpRequest(
      uri = s"https://lichess.org/api/bot/game/stream/$gameId",
      headers = List(authHeader)
    )
    
    Source.futureSource(
      Http().singleRequest(request).map { response =>
        if response.status.isSuccess() then
          response.entity.dataBytes
            .via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 65536, allowTruncation = true))
            .map(_.utf8String.trim)
            .filter(_.nonEmpty)
            .map(_.parseJson.convertTo[GameEvent])
        else
          response.discardEntityBytes()
          Source.failed(new RuntimeException(s"Failed to connect to game stream $gameId: ${response.status}"))
      }
    )

  /**
   * Submits a move for a game in UCI format (e.g. "e2e4").
   */
  def makeMove(gameId: String, moveUci: String): Future[Unit] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"https://lichess.org/api/bot/game/$gameId/move/$moveUci",
      headers = List(authHeader)
    )
    Http().singleRequest(request).map { res =>
      res.discardEntityBytes()
      if !res.status.isSuccess() then throw new RuntimeException(s"Failed to make move: ${res.status}")
    }
