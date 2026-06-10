package ch.tichess.streaming

import akka.NotUsed
import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.{Flow, Sink, Source}
import ch.tichess.controller.Command
import ch.tichess.view.CommandResponse

import scala.concurrent.{ExecutionContext, Future}

final case class StreamCommandResult(
    line: Long,
    input: String,
    accepted: Boolean,
    success: Boolean,
    message: Option[String],
    fen: Option[String]
)

private[streaming] final case class IndexedCommand(line: Long, input: String)

private[streaming] enum ValidatedCommand:
  case Accepted(command: IndexedCommand)
  case Rejected(command: IndexedCommand, reason: String)

object ChessCommandStream:

  val cleanupFlow: Flow[(String, Long), IndexedCommand, NotUsed] =
    Flow[(String, Long)]
      .map { case (raw, index) => IndexedCommand(index + 1, raw.trim) }
      .filter(command =>
        command.input.nonEmpty &&
          !command.input.startsWith("#") &&
          !command.input.startsWith("//")
      )

  val validationFlow: Flow[IndexedCommand, ValidatedCommand, NotUsed] =
    Flow[IndexedCommand].map { command =>
      Command.parse(command.input) match
        case Right(_)     => ValidatedCommand.Accepted(command)
        case Left(reason) => ValidatedCommand.Rejected(command, reason)
    }

  def processingFlow(
      execute: String => Future[CommandResponse]
  )(using ExecutionContext): Flow[ValidatedCommand, StreamCommandResult, NotUsed] =
    Flow[ValidatedCommand].mapAsync(1) {
      case ValidatedCommand.Accepted(command) =>
        execute(command.input)
          .map(response =>
            StreamCommandResult(
              line = command.line,
              input = command.input,
              accepted = true,
              success = response.success,
              message = response.message,
              fen = response.fen
            )
          )
          .recover { case error =>
            StreamCommandResult(
              line = command.line,
              input = command.input,
              accepted = true,
              success = false,
              message = Some(error.getMessage),
              fen = None
            )
          }

      case ValidatedCommand.Rejected(command, reason) =>
        Future.successful(
          StreamCommandResult(
            line = command.line,
            input = command.input,
            accepted = false,
            success = false,
            message = Some(reason),
            fen = None
          )
        )
    }

  def source(lines: Iterable[String]): Source[ValidatedCommand, NotUsed] =
    Source(lines.toList)
      .zipWithIndex
      .via(cleanupFlow)
      .via(validationFlow)

  def run(
      lines: Iterable[String],
      execute: String => Future[CommandResponse]
  )(using ActorSystem[?], ExecutionContext): Future[Seq[StreamCommandResult]] =
    source(lines)
      .via(processingFlow(execute))
      .runWith(Sink.seq)

  def runText(
      text: String,
      execute: String => Future[CommandResponse]
  )(using ActorSystem[?], ExecutionContext): Future[Seq[StreamCommandResult]] =
    run(text.linesIterator.toList, execute)

  def processOne(
      input: String,
      execute: String => Future[CommandResponse]
  )(using ActorSystem[?], ExecutionContext): Future[StreamCommandResult] =
    run(List(input), execute).map(
      _.headOption.getOrElse(
        StreamCommandResult(
          line = 1,
          input = input.trim,
          accepted = false,
          success = false,
          message = Some("The command was empty or a comment."),
          fen = None
        )
      )
    )
