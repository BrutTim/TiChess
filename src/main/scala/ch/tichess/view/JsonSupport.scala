package ch.tichess.view

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

final case class MoveRequest(fen: String, algebraicMove: String) // e.g. "e2 e4" or "e7 e8 q"
final case class ModelResponse(success: Boolean, fen: Option[String], error: Option[String])

final case class CommandRequest(input: String)
final case class CommandResponse(success: Boolean, message: Option[String], fen: Option[String], quit: Boolean)

final case class StateResponse(fen: String, statusText: String, isGameOver: Boolean, drawOffered: Boolean, whiteCaptured: String, blackCaptured: String)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol:
  implicit val moveRequestFormat: RootJsonFormat[MoveRequest] = jsonFormat2(MoveRequest.apply)
  implicit val modelResponseFormat: RootJsonFormat[ModelResponse] = jsonFormat3(ModelResponse.apply)
  
  implicit val commandRequestFormat: RootJsonFormat[CommandRequest] = jsonFormat1(CommandRequest.apply)
  implicit val commandResponseFormat: RootJsonFormat[CommandResponse] = jsonFormat4(CommandResponse.apply)
  
  implicit val stateResponseFormat: RootJsonFormat[StateResponse] = jsonFormat6(StateResponse.apply)
