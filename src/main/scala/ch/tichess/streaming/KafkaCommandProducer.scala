package ch.tichess.streaming

import akka.Done
import akka.actor.typed.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.{Sink, Source}
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer

import scala.concurrent.{ExecutionContext, Future}

final case class KafkaPublishResponse(topic: String, published: Int, rejected: List[StreamCommandResult])

final class KafkaCommandProducer(
    bootstrapServers: String,
    topic: String
)(using system: ActorSystem[?], ec: ExecutionContext):

  private val settings =
    ProducerSettings(system, new StringSerializer, new StringSerializer)
      .withBootstrapServers(bootstrapServers)

  def publishText(text: String): Future[KafkaPublishResponse] =
    ChessCommandStream
      .source(text.linesIterator.toList)
      .runWith(Sink.seq)
      .flatMap { commands =>
        val accepted = commands.collect {
          case ValidatedCommand.Accepted(command) => command
        }
        val rejected = commands.collect {
          case ValidatedCommand.Rejected(command, reason) =>
            StreamCommandResult(
              line = command.line,
              input = command.input,
              accepted = false,
              success = false,
              message = Some(reason),
              fen = None
            )
        }.toList

        Source(accepted)
          .map(command => new ProducerRecord[String, String](topic, "default", command.input))
          .runWith(Producer.plainSink(settings))
          .map(_ => KafkaPublishResponse(topic, accepted.size, rejected))
      }
