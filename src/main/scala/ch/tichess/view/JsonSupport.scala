package ch.tichess.view

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import ch.tichess.controller.persistence.ChallengeRecord
import ch.tichess.streaming.{KafkaPublishResponse, StreamCommandResult}
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

final case class MoveRequest(fen: String, algebraicMove: String) // e.g. "e2 e4" or "e7 e8 q"
final case class ModelResponse(success: Boolean, fen: Option[String], error: Option[String])

final case class CommandRequest(input: String)
final case class CommandResponse(success: Boolean, message: Option[String], fen: Option[String], quit: Boolean)

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
  implicit val challengeRecordFormat: RootJsonFormat[ChallengeRecord] = jsonFormat4(ChallengeRecord.apply)
  implicit val challengeRecordListFormat: RootJsonFormat[List[ChallengeRecord]] = listFormat[ChallengeRecord]

  implicit val stateResponseFormat: RootJsonFormat[StateResponse] = jsonFormat15(StateResponse.apply)
  implicit val streamCommandResultFormat: RootJsonFormat[StreamCommandResult] = jsonFormat6(StreamCommandResult.apply)
  implicit val streamCommandResultListFormat: RootJsonFormat[List[StreamCommandResult]] = listFormat[StreamCommandResult]
  implicit val kafkaPublishResponseFormat: RootJsonFormat[KafkaPublishResponse] = jsonFormat3(KafkaPublishResponse.apply)
