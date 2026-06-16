package ch.tichess.streaming

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import ch.tichess.view.CommandResponse
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AsyncFunSuite

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

final class ChessCommandStreamSpec extends AsyncFunSuite with BeforeAndAfterAll:

  private given system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "ChessCommandStreamSpec")
  private given scala.concurrent.ExecutionContext = system.executionContext

  override def afterAll(): Unit =
    system.terminate()

  test("reactive stream cleans, validates, processes and collects command DSL lines") {
    val calls = ListBuffer.empty[String]
    val script =
      """# opening
        |e2 e4
        |
        |invalid command
        |// ignored
        |e7 e5
        |""".stripMargin

    def execute(input: String): Future[CommandResponse] =
      calls += input
      Future.successful(
        CommandResponse(success = true, message = Some(s"processed $input"), fen = Some("fen"), quit = false)
      )

    ChessCommandStream.runText(script, execute).map { results =>
      assert(calls.toList == List("e2 e4", "e7 e5"))
      assert(results.map(_.line) == Seq(2, 4, 6))
      assert(results.head.accepted)
      assert(results.head.success)
      assert(!results(1).accepted)
      assert(!results(1).success)
      assert(results(2).message.contains("processed e7 e5"))
    }
  }

  test("processing flow converts downstream failures into stream results") {
    ChessCommandStream
      .run(List("e2 e4"), _ => Future.failed(new RuntimeException("controller offline")))
      .map { results =>
        assert(results.size == 1)
        assert(results.head.accepted)
        assert(!results.head.success)
        assert(results.head.message.contains("controller offline"))
      }
  }

  test("processOne reports empty and comment-only commands without calling the controller") {
    var called = false
    ChessCommandStream
      .processOne("  # ignored  ", _ => {
        called = true
        Future.successful(CommandResponse(true, None, None, false))
      })
      .map { result =>
        assert(!called)
        assert(!result.accepted)
        assert(result.message.contains("The command was empty or a comment."))
      }
  }
