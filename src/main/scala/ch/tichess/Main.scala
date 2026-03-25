package ch.tichess

import ch.tichess.controller.Controller
import ch.tichess.view.ConsoleView

object Main:
  def main(args: Array[String]): Unit =
    mainWith(ConsoleApp.LiveStdIO, args)

  def mainWith(io: ConsoleApp.IO, args: Array[String]): Unit =
    if args.nonEmpty then ConsoleApp.run(ConsoleApp.ScriptIO(args.toList, io.writeLine))
    else ConsoleApp.run(io)

object ConsoleApp:
  trait IO:
    def readLine(): Option[String]
    def writeLine(s: String): Unit

  final case class StdIO(read: () => String | Null, write: String => Unit) extends IO:
    override def readLine(): Option[String] = Option(read())
    override def writeLine(s: String): Unit = write(s)

  val LiveStdIO: StdIO =
    StdIO(
      () => scala.io.StdIn.readLine(),
      (s: String) => println(s)
    )

  final case class ScriptIO(lines: List[String], out: String => Unit) extends IO:
    private val it = lines.iterator
    override def readLine(): Option[String] =
      if it.hasNext then Some(it.next()) else None
    override def writeLine(s: String): Unit = out(s)

  def run(io: IO): Unit =
    loop(io, Controller.initial, None)

  @annotation.tailrec
  private def loop(io: IO, game: ch.tichess.model.Game, message: Option[String]): Unit =
    io.writeLine(ConsoleView.render(game, message))
    io.readLine() match
      case None => ()
      case Some(in) =>
        val res = Controller.update(game, in)
        if res.quit then io.writeLine(ConsoleView.render(res.game, res.message))
        else loop(io, res.game, res.message)

