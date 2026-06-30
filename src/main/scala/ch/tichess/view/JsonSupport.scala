package ch.tichess.view

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import ch.tichess.analytics.{GameEvent, PlayerStatistics}
import ch.tichess.controller.persistence.ChallengeRecord
import ch.tichess.streaming.{KafkaPublishResponse, StreamCommandResult}
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

final case class MoveRequest(fen: String, algebraicMove: String) // e.g. "e2 e4" or "e7 e8 q"
final case class ModelResponse(success: Boolean, fen: Option[String], error: Option[String])

final case class CommandRequest(input: String)
final case class CommandResponse(success: Boolean, message: Option[String], fen: Option[String], quit: Boolean)
final case class TournamentListRequest(baseUrl: String, token: Option[String])
final case class TournamentDetailRequest(baseUrl: String, token: Option[String], tournamentId: String)
final case class TournamentRoundRequest(baseUrl: String, token: Option[String], tournamentId: String, round: Int)
final case class TournamentStreamRequest(baseUrl: String, token: String, tournamentId: String)
final case class TournamentGameStreamRequest(baseUrl: String, token: String, tournamentId: String, gameId: String)
final case class TournamentProxyResponse(success: Boolean, status: Int, body: String, error: Option[String])

final case class StateResponse(
    fen: String,
    statusText: String,
    isGameOver: Boolean,
    drawOffered: Boolean,
    whiteCaptured: String,
    blackCaptured: String,
    moveList: List[String],
    legalMoves: Map[String, List[String]],
    lastMoveFrom: Option[String],
    lastMoveTo: Option[String],
    currentParser: String,
    availableParsers: List[String],
    challengeHintFrom: Option[String],
    challengeSideToMove: Option[String],
    activeBot: Option[String]
)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol:
  implicit val moveRequestFormat: RootJsonFormat[MoveRequest] = jsonFormat2(MoveRequest.apply)
  implicit val modelResponseFormat: RootJsonFormat[ModelResponse] = jsonFormat3(ModelResponse.apply)

  implicit val commandRequestFormat: RootJsonFormat[CommandRequest] = jsonFormat1(CommandRequest.apply)
  implicit val commandResponseFormat: RootJsonFormat[CommandResponse] = jsonFormat4(CommandResponse.apply)
  implicit val tournamentListRequestFormat: RootJsonFormat[TournamentListRequest] = jsonFormat2(TournamentListRequest.apply)
  implicit val tournamentDetailRequestFormat: RootJsonFormat[TournamentDetailRequest] = jsonFormat3(TournamentDetailRequest.apply)
  implicit val tournamentRoundRequestFormat: RootJsonFormat[TournamentRoundRequest] = jsonFormat4(TournamentRoundRequest.apply)
  implicit val tournamentStreamRequestFormat: RootJsonFormat[TournamentStreamRequest] = jsonFormat3(TournamentStreamRequest.apply)
  implicit val tournamentGameStreamRequestFormat: RootJsonFormat[TournamentGameStreamRequest] = jsonFormat4(TournamentGameStreamRequest.apply)
  implicit val tournamentProxyResponseFormat: RootJsonFormat[TournamentProxyResponse] = jsonFormat4(TournamentProxyResponse.apply)
  implicit val challengeRecordFormat: RootJsonFormat[ChallengeRecord] = jsonFormat4(ChallengeRecord.apply)
  implicit val challengeRecordListFormat: RootJsonFormat[List[ChallengeRecord]] = listFormat[ChallengeRecord]

  implicit val stateResponseFormat: RootJsonFormat[StateResponse] = jsonFormat15(StateResponse.apply)
  implicit val streamCommandResultFormat: RootJsonFormat[StreamCommandResult] = jsonFormat6(StreamCommandResult.apply)
  implicit val streamCommandResultListFormat: RootJsonFormat[List[StreamCommandResult]] = listFormat[StreamCommandResult]
  implicit val kafkaPublishResponseFormat: RootJsonFormat[KafkaPublishResponse] = jsonFormat3(KafkaPublishResponse.apply)
  implicit val gameEventFormat: RootJsonFormat[GameEvent] = jsonFormat9(GameEvent.apply)
  implicit val playerStatisticsFormat: RootJsonFormat[PlayerStatistics] = jsonFormat7(PlayerStatistics.apply)
  implicit val playerStatisticsListFormat: RootJsonFormat[List[PlayerStatistics]] = listFormat[PlayerStatistics]
