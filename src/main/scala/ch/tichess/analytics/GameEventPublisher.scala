package ch.tichess.analytics

import akka.Done
import akka.actor.typed.ActorSystem
import akka.kafka.ProducerSettings
import akka.kafka.scaladsl.Producer
import akka.stream.scaladsl.Source
import ch.tichess.view.JsonSupport
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.StringSerializer
import spray.json.*

import scala.concurrent.{ExecutionContext, Future}

trait GameEventPublisher:
  def publish(event: GameEvent): Future[Done]

final class KafkaGameEventPublisher(
    bootstrapServers: String,
    topic: String
)(using system: ActorSystem[?], ec: ExecutionContext)
    extends GameEventPublisher
    with JsonSupport:

  private val settings =
    ProducerSettings(system, new StringSerializer, new StringSerializer)
      .withBootstrapServers(bootstrapServers)

  override def publish(event: GameEvent): Future[Done] =
    Source
      .single(
        new ProducerRecord[String, String](
          topic,
          event.gameId,
          event.toJson.compactPrint
        )
      )
      .runWith(Producer.plainSink(settings))
