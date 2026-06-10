package ch.tichess

import ch.tichess.controller.Controller
import ch.tichess.view.ConsoleView
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import ch.tichess.bot.lichess.OutgoingChallenge
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.Duration

object Main:
  def main(args: Array[String]): Unit =
    if args.contains("bot") then
      startBotMode(args)
    else
      mainWith(ConsoleApp.LiveStdIO, args)

  def mainWith(io: ConsoleApp.IO, args: Array[String]): Unit =
    if args.nonEmpty && !args.contains("bot") then ConsoleApp.run(ConsoleApp.ScriptIO(args.toList, io.writeLine))
    else ConsoleApp.run(io)

  private def startBotMode(args: Array[String]): Unit =
    val token = sys.env.getOrElse("LICHESS_TOKEN", {
      System.err.println("Error: LICHESS_TOKEN environment variable is not set.")
      sys.exit(1)
    })

    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "LichessBotSystem")
    implicit val ec: ExecutionContext = system.executionContext

    val client = new ch.tichess.bot.lichess.LichessClient(token)
    val bot = new ch.tichess.bot.AlphaBetaBot(10000L, Some(Controller.openingDb))
    
    val runner = new ch.tichess.bot.lichess.LichessBotRunner(client, bot)
    runner.start()

    parseOutgoingChallenge(args.toList).foreach { challenge =>
      println(s"Creating Lichess challenge against ${challenge.username} (${challenge.clockLimitSeconds}+${challenge.clockIncrementSeconds}, ${challenge.color}, ${if challenge.rated then "rated" else "casual"})...")
      client.challengeUser(challenge).onComplete {
        case scala.util.Success(_) =>
          println(s"Challenge sent to ${challenge.username}.")
        case scala.util.Failure(e) =>
          println(s"Failed to create challenge against ${challenge.username}: ${e.getMessage}")
      }
    }
    
    sys.addShutdownHook {
      system.terminate()
    }
    println("Bot is running. Stop it with SIGTERM or CTRL+C.")
    Await.result(system.whenTerminated, Duration.Inf)

  private def parseOutgoingChallenge(args: List[String]): Option[OutgoingChallenge] =
    val challengeIndex = args.indexWhere(arg => arg == "challenge" || arg == "--challenge")
    if challengeIndex < 0 || challengeIndex + 1 >= args.length then None
    else
      val username = args(challengeIndex + 1)
      val rated = args.contains("--rated")
      val color = optionValue(args, "--color").getOrElse("random")
      val variant = optionValue(args, "--variant").getOrElse("standard")
      val (clockLimit, clockIncrement) =
        optionValue(args, "--clock").flatMap(parseClock).getOrElse((180, 2))

      Some(OutgoingChallenge(username, rated, clockLimit, clockIncrement, color, variant))

  private def optionValue(args: List[String], flag: String): Option[String] =
    val index = args.indexOf(flag)
    if index >= 0 && index + 1 < args.length then Some(args(index + 1)) else None

  private def parseClock(value: String): Option[(Int, Int)] =
    value.split("\\+", 2).toList match
      case limit :: increment :: Nil =>
        for
          limitSeconds <- limit.toIntOption
          incrementSeconds <- increment.toIntOption
        yield (limitSeconds, incrementSeconds)
      case _ => None

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
