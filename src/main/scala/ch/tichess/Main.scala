package ch.tichess

import ch.tichess.controller.Controller
import ch.tichess.view.ConsoleView
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import scala.concurrent.ExecutionContext

object Main:
  def main(args: Array[String]): Unit =
    if args.contains("bot") then
      startBotMode()
    else
      mainWith(ConsoleApp.LiveStdIO, args)

  def mainWith(io: ConsoleApp.IO, args: Array[String]): Unit =
    if args.nonEmpty && !args.contains("bot") then ConsoleApp.run(ConsoleApp.ScriptIO(args.toList, io.writeLine))
    else ConsoleApp.run(io)

  private def startBotMode(): Unit =
    val token = sys.env.getOrElse("LICHESS_TOKEN", {
      System.err.println("Error: LICHESS_TOKEN environment variable is not set.")
      sys.exit(1)
    })

    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "LichessBotSystem")
    implicit val ec: ExecutionContext = system.executionContext

    val client = new ch.tichess.bot.lichess.LichessClient(token)
    val bot = new ch.tichess.bot.AlphaBetaBot(5000L, Some(Controller.openingDb))
    
    val runner = new ch.tichess.bot.lichess.LichessBotRunner(client, bot)
    runner.start()
    
    // Keep application alive
    scala.io.StdIn.readLine("Bot is running. Press ENTER to stop...\n")
    system.terminate()

object ConsoleApp:
  trait IO:
    def readLine(): Option[String]
    def writeLine(s: String): Unit

  final case class StdIO(read: () => Option[String], write: String => Unit) extends IO:
    override def readLine(): Option[String] = read()
    override def writeLine(s: String): Unit = write(s)

  val LiveStdIO: StdIO =
    StdIO(
      () => Option(scala.io.StdIn.readLine()),
      (s: String) => println(s)
    )

  final case class ScriptIO(lines: List[String], out: String => Unit) extends IO:
    private val it = lines.iterator
    override def readLine(): Option[String] =
      if it.hasNext then Some(it.next()) else None
    override def writeLine(s: String): Unit = out(s)

  def run(io: IO): Unit =
    loop(io, Controller.initialState, None)

  @annotation.tailrec
  private def loop(io: IO, state: ch.tichess.controller.AppState, message: Option[String]): Unit =
    io.writeLine(ConsoleView.render(state.game, message, state.startGame))
    io.readLine() match
      case None => ()
      case Some(in) =>
        val res = Controller.update(state, in)
        if res.quit then io.writeLine(ConsoleView.render(res.game, res.message, res.state.startGame))
        else loop(io, res.state, res.message)
