package ch.tichess.streaming

import akka.Done
import akka.actor.typed.ActorSystem
import akka.kafka.scaladsl.Consumer.DrainingControl
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.kafka.{CommitterSettings, ConsumerSettings, ProducerMessage, ProducerSettings, Subscriptions}
import akka.stream.scaladsl.Keep
import ch.tichess.view.{CommandResponse, JsonSupport}
import org.apache.kafka.clients.consumer.ConsumerConfig
import org.apache.kafka.clients.producer.ProducerRecord
import org.apache.kafka.common.serialization.{StringDeserializer, StringSerializer}
import spray.json.*

import scala.concurrent.{ExecutionContext, Future}

final class KafkaCommandBridge(
    bootstrapServers: String,
    commandsTopic: String,
    eventsTopic: String,
    groupId: String,
    execute: String => Future[CommandResponse]
)(using system: ActorSystem[?], ec: ExecutionContext)
    extends JsonSupport:

  private val consumerSettings =
    ConsumerSettings(system, new StringDeserializer, new StringDeserializer)
      .withBootstrapServers(bootstrapServers)
      .withGroupId(groupId)
      .withProperty(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest")

  private val producerSettings =
    ProducerSettings(system, new StringSerializer, new StringSerializer)
      .withBootstrapServers(bootstrapServers)

  private val committerSettings = CommitterSettings(system).withMaxBatch(1)

  def start(): DrainingControl[Done] =
    Consumer
      .committableSource(consumerSettings, Subscriptions.topics(commandsTopic))
      .mapAsync(1) { message =>
        ChessCommandStream.processOne(message.record.value, execute).map { result =>
          ProducerMessage.single(
            new ProducerRecord[String, String](
              eventsTopic,
              Option(message.record.key).getOrElse("default"),
              result.toJson.compactPrint
            ),
            message.committableOffset
          )
        }
      }
      .toMat(Producer.committableSink(producerSettings, committerSettings))(DrainingControl.apply)
      .run()
