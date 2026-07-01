package ch.tichess.bot.tournament

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken, RawHeader}
import akka.stream.scaladsl.{Framing, Source}
import akka.util.ByteString
import spray.json.*

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*

final case class TournamentRegisterRequest(name: String, isBot: Boolean)
final case class TournamentRegisterResponse(id: String, token: String)
final case class TournamentOk(ok: Boolean)
final case class TournamentClock(whiteTime: Double, blackTime: Double, increment: Option[Double] = None)
final case class TournamentPlayer(id: Option[String] = None, name: Option[String] = None)
final case class TournamentEvent(
    `type`: String,
    round: Option[Int] = None,
    gameId: Option[String] = None,
    color: Option[String] = None,
    white: Option[TournamentPlayer] = None,
    black: Option[TournamentPlayer] = None
)
final case class TournamentGameEvent(
    `type`: String,
    fen: Option[String] = None,
    moves: Option[String] = None,
    turn: Option[String] = None,
    clock: Option[TournamentClock] = None,
    winner: Option[String] = None,
    status: Option[String] = None,
    uci: Option[String] = None
)

trait TournamentJsonProtocol extends DefaultJsonProtocol:
  implicit val registerRequestFormat: RootJsonFormat[TournamentRegisterRequest] = jsonFormat2(TournamentRegisterRequest.apply)
  implicit val registerResponseFormat: RootJsonFormat[TournamentRegisterResponse] = jsonFormat2(TournamentRegisterResponse.apply)
  implicit val okFormat: RootJsonFormat[TournamentOk] = jsonFormat1(TournamentOk.apply)
  implicit object clockFormat extends RootJsonFormat[TournamentClock]:
    override def write(clock: TournamentClock): JsValue =
      JsObject(
        Map(
          "whiteTime" -> JsNumber(clock.whiteTime),
          "blackTime" -> JsNumber(clock.blackTime)
        ) ++ clock.increment.map(value => "increment" -> JsNumber(value))
      )

    override def read(json: JsValue): TournamentClock =
      val fields = json.asJsObject.fields
      TournamentClock(
        whiteTime = fields("whiteTime").convertTo[Double],
        blackTime = fields("blackTime").convertTo[Double],
        increment = fields.get("increment").map(_.convertTo[Double])
      )

  implicit object tournamentPlayerFormat extends RootJsonFormat[TournamentPlayer]:
    override def write(player: TournamentPlayer): JsValue =
      JsObject(
        Map.empty[String, JsValue] ++
          player.id.map(value => "id" -> JsString(value)) ++
          player.name.map(value => "name" -> JsString(value))
      )

    override def read(json: JsValue): TournamentPlayer =
      json match
        case JsString(name) => TournamentPlayer(name = Some(name))
        case JsObject(fields) =>
          TournamentPlayer(
            id = firstString(fields, "id", "userId", "botId", "_id"),
            name = firstString(fields, "name", "username", "displayName", "userName")
          )
        case _ => TournamentPlayer()

  implicit object tournamentEventFormat extends RootJsonFormat[TournamentEvent]:
    override def write(event: TournamentEvent): JsValue =
      JsObject(
        Map("type" -> JsString(event.`type`)) ++
          event.round.map(value => "round" -> JsNumber(value)) ++
          event.gameId.map(value => "gameId" -> JsString(value)) ++
          event.color.map(value => "color" -> JsString(value)) ++
          event.white.map(value => "white" -> value.toJson) ++
          event.black.map(value => "black" -> value.toJson)
      )

    override def read(json: JsValue): TournamentEvent =
      val fields = json.asJsObject.fields
      val players = fields.get("players").collect { case JsObject(values) => values }.getOrElse(Map.empty)
      TournamentEvent(
        `type` = fields("type").convertTo[String],
        round = fields.get("round").map(_.convertTo[Int]),
        gameId = firstString(fields, "gameId", "gameID", "id"),
        color = firstString(fields, "color", "botColor"),
        white = playerFrom(fields, players, "white"),
        black = playerFrom(fields, players, "black")
      )

  implicit val gameEventFormat: RootJsonFormat[TournamentGameEvent] = jsonFormat8(TournamentGameEvent.apply)

  private def firstString(fields: Map[String, JsValue], names: String*): Option[String] =
    names.iterator.flatMap(name => fields.get(name).flatMap(asString)).find(_.nonEmpty)

  private def asString(value: JsValue): Option[String] =
    value match
      case JsString(text) => Some(text)
      case JsNumber(number) => Some(number.toString)
      case _ => None

  private def playerFrom(
      fields: Map[String, JsValue],
      players: Map[String, JsValue],
      color: String
  ): Option[TournamentPlayer] =
    fields
      .get(color)
      .orElse(players.get(color))
      .map(_.convertTo[TournamentPlayer])
      .orElse {
        val id = firstString(fields, s"${color}Id", s"${color}ID", s"${color}UserId", s"${color}BotId")
        val name = firstString(fields, s"${color}Name", s"${color}Username", s"${color}UserName")
        Option.when(id.nonEmpty || name.nonEmpty)(TournamentPlayer(id, name))
      }

class TournamentClient(baseUrl: String, token: String)(implicit system: ActorSystem[?], ec: ExecutionContext) extends TournamentJsonProtocol:
  private val cleanBaseUrl = baseUrl.stripSuffix("/")
  private val authHeader = Authorization(OAuth2BearerToken(token))
  private val ndjsonAccept = RawHeader("Accept", "application/x-ndjson")

  def joinTournament(tournamentId: String): Future[Unit] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$cleanBaseUrl/api/tournament/$tournamentId/join",
      headers = List(authHeader)
    )
    Http().singleRequest(request).flatMap(requireSuccess(_, s"join tournament $tournamentId"))

  def streamTournament(tournamentId: String): Source[TournamentEvent, ?] =
    val request = HttpRequest(
      uri = s"$cleanBaseUrl/api/tournament/$tournamentId/stream",
      headers = List(authHeader, ndjsonAccept)
    )
    ndjson(request, s"connect to tournament stream $tournamentId").map(_.parseJson.convertTo[TournamentEvent])

  def streamGame(tournamentId: String, gameId: String): Source[TournamentGameEvent, ?] =
    val request = HttpRequest(
      uri = s"$cleanBaseUrl/api/tournament/$tournamentId/game/$gameId/stream",
      headers = List(authHeader, ndjsonAccept)
    )
    ndjson(request, s"connect to game stream $gameId").map(_.parseJson.convertTo[TournamentGameEvent])

  def makeMove(tournamentId: String, gameId: String, moveUci: String): Future[Unit] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"$cleanBaseUrl/api/tournament/$tournamentId/game/$gameId/move/$moveUci",
      headers = List(authHeader)
    )
    Http().singleRequest(request).flatMap(requireSuccess(_, s"submit move $moveUci for game $gameId"))

  private def ndjson(request: HttpRequest, action: String): Source[String, ?] =
    Source.futureSource(
      Http().singleRequest(request).map { response =>
        if response.status.isSuccess() then
          response.entity.dataBytes
            .via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 65536, allowTruncation = true))
            .map(_.utf8String.trim)
            .filter(_.nonEmpty)
        else
          response.discardEntityBytes()
          Source.failed(new RuntimeException(s"Failed to $action: ${response.status}"))
      }
    )

  private def requireSuccess(response: HttpResponse, action: String): Future[Unit] =
    if response.status.isSuccess() then
      response.discardEntityBytes()
      Future.unit
    else
      response.entity.toStrict(2.seconds).map { strict =>
        val body = strict.data.utf8String.trim
        val detail = if body.nonEmpty then s": $body" else ""
        throw new RuntimeException(s"Failed to $action: ${response.status}$detail")
      }

object TournamentClient extends TournamentJsonProtocol:
  def registerBot(baseUrl: String, name: String)(implicit system: ActorSystem[?], ec: ExecutionContext): Future[TournamentRegisterResponse] =
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = s"${baseUrl.stripSuffix("/")}/api/auth/register",
      entity = HttpEntity(ContentTypes.`application/json`, TournamentRegisterRequest(name, isBot = true).toJson.compactPrint)
    )
    Http().singleRequest(request).flatMap { response =>
      if response.status == StatusCodes.Created then
        response.entity.toStrict(5.seconds).map(_.data.utf8String.parseJson.convertTo[TournamentRegisterResponse])
      else
        response.entity.toStrict(2.seconds).map { strict =>
          val body = strict.data.utf8String.trim
          val detail = if body.nonEmpty then s": $body" else ""
          throw new RuntimeException(s"Failed to register tournament bot: ${response.status}$detail")
        }
    }
