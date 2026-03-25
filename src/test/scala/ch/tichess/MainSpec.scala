package ch.tichess

import ch.tichess.Main
import org.scalatest.funsuite.AnyFunSuite

final class MainSpec extends AnyFunSuite:

  test("ConsoleApp.run handles quit and prints final message") {
    val inputs = List("quit")
    val io = new FakeIO(inputs)
    ConsoleApp.run(io)
    assert(io.outputs.nonEmpty)
    assert(io.outputs.last.contains("Bye."))
  }

  test("ConsoleApp.run recurses on non-quit command") {
    val inputs = List("help")
    val io = new FakeIO(inputs)
    ConsoleApp.run(io)
    assert(io.outputs.size >= 2) // initial + after help
  }

  test("ConsoleApp.run stops on null input") {
    val io = new FakeIO(Nil)
    ConsoleApp.run(io)
    assert(io.outputs.size == 1) // initial board render only
  }

  test("Main.main can run scripted args without blocking") {
    Main.main(Array("quit"))
  }

  test("StdIO delegates to provided functions") {
    var readCalled = 0
    var wrote: List[String] = Nil
    val io = ConsoleApp.StdIO(
      () =>
        readCalled += 1
        "x",
      s => wrote = wrote :+ s
    )
    assert(io.readLine() == Some("x"))
    io.writeLine("y")
    assert(readCalled == 1)
    assert(wrote == List("y"))
    // also touch LiveStdIO value so its initialization is covered
    assert(ConsoleApp.LiveStdIO.isInstanceOf[ConsoleApp.StdIO])
  }

  test("Main.mainWith runs interactive branch with injected IO") {
    val io = new FakeIO(List("quit"))
    Main.mainWith(io, Array.empty)
    assert(io.outputs.last.contains("Bye."))
  }

  test("ScriptIO readLine covers Nil and non-Nil branches") {
    val out: String => Unit = _ => ()
    val io = ConsoleApp.ScriptIO(List("a"), out)
    assert(io.readLine() == Some("a"))
    assert(io.readLine().isEmpty)
    io.writeLine("x") // cover writeLine
  }

  test("LiveStdIO write lambda can be invoked") {
    // This will print one line during tests, but ensures the internal write lambda is executed.
    ConsoleApp.LiveStdIO.writeLine("test-output")
  }

  test("LiveStdIO read lambda can be invoked without blocking") {
    scala.Console.withIn(new java.io.StringReader("hello\n")) {
      assert(ConsoleApp.LiveStdIO.readLine().contains("hello"))
    }
  }

private final class FakeIO(lines: List[String]) extends ConsoleApp.IO:
  private val it = lines.iterator
  private var wrote: Vector[String] = Vector.empty

  def outputs: Vector[String] = wrote

  override def readLine(): Option[String] =
    if it.hasNext then Some(it.next()) else None

  override def writeLine(s: String): Unit =
    wrote = wrote :+ s

