package ch.tichess.bot.tournament

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.Sink
import ch.tichess.bot.ChessBot
import ch.tichess.controller.AppState
import ch.tichess.model.*

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.{ExecutionContext, Future}

class TournamentBotRunner(client: TournamentClient, tournamentId: String, bot: ChessBot)(implicit system: ActorSystem[?], ec: ExecutionContext):
  private val startedGames = ConcurrentHashMap.newKeySet[String]()

  def start(join: Boolean): Unit =
    println(s">>> Starting Tournament Bot Runner for tournament $tournamentId...")
    if join then
      client.joinTournament(tournamentId).onComplete {
        case scala.util.Success(_) => println(s"Joined tournament $tournamentId.")
        case scala.util.Failure(e) => println(s"Could not join tournament $tournamentId: ${e.getMessage}")
      }

    client.streamTournament(tournamentId).runWith(Sink.foreach {
      case TournamentEvent("gameStart", round, Some(gameId), Some(color)) =>
        val botColor = parseColor(color)
        if startedGames.add(gameId) then
          println(s"Tournament game started: $gameId, round ${round.getOrElse(0)}, bot plays $botColor")
          startGameLoop(gameId, botColor)
        else
          println(s"Tournament game $gameId is already connected; ignoring replayed gameStart.")
      case TournamentEvent("roundStarted", round, _, _) =>
        println(s"Tournament round ${round.getOrElse(0)} started.")
      case TournamentEvent("roundFinished", round, _, _) =>
        println(s"Tournament round ${round.getOrElse(0)} finished.")
      case TournamentEvent("tournamentStarted", _, _, _) =>
        println("Tournament started.")
      case TournamentEvent("tournamentFinished", _, _, _) =>
        println("Tournament finished.")
      case TournamentEvent("heartbeat", _, _, _) =>
        ()
      case other =>
        println(s"Ignoring tournament event ${other.`type`}.")
    }).failed.foreach(e => println(s"Tournament stream failed: ${e.getMessage}"))

  private def startGameLoop(gameId: String, botColor: Color): Unit =
    val currentState = AtomicReference[AppState](AppState(Game.initial))
    val thinking = AtomicBoolean(false)

    client.streamGame(tournamentId, gameId).runWith(Sink.foreach { event =>
      event.`type` match
        case "gameState" | "move" =>
          event.fen match
            case Some(fen) =>
              Fen.parse(fen) match
                case Right(game) =>
                  val previous = currentState.get()
                  val history = event.moves
                    .map(parseMoves)
                    .getOrElse(previous.moveHistory ++ event.uci.flatMap(parseUciMove))
                  val state = AppState(game, startGame = previous.startGame, moveHistory = history)
                  currentState.set(state)
                  val timeMs = timeFor(event.clock, botColor)
                  val incrementMs = event.clock.flatMap(_.increment).map(seconds => Math.max(0L, (seconds * 1000.0).toLong))
                  println(s"Game $gameId: ${event.`type`} (${event.status.getOrElse("ongoing")}), turn ${game.sideToMove}.")
                  checkTurnAndPlay(gameId, state, botColor, timeMs, incrementMs, thinking, () => currentState.get())
                case Left(err) =>
                  println(s"Game $gameId: Could not parse tournament FEN '$fen': $err")
            case None =>
              println(s"Game $gameId: ${event.`type`} without FEN ignored.")
        case "gameEnd" =>
          println(s"Game $gameId ended with status ${event.status.getOrElse("?")}, winner ${event.winner.getOrElse("draw")}.")
        case "heartbeat" =>
          ()
        case other =>
          println(s"Game $gameId: Ignoring game event $other.")
    }).failed.foreach(e => println(s"Tournament game loop $gameId failed: ${e.getMessage}"))

  private def checkTurnAndPlay(
      gameId: String,
      state: AppState,
      botColor: Color,
      timeMs: Option[Long],
      incrementMs: Option[Long],
      thinking: AtomicBoolean,
      currentState: () => AppState
  ): Unit =
    if state.game.isCheckmate || state.game.isDraw then
      println(s"Game $gameId is over.")
    else if state.game.sideToMove == botColor then
      if !thinking.compareAndSet(false, true) then
        println(s"Game $gameId: Bot is already thinking; skipping duplicate turn update.")
      else
        val searchFen = Fen.encodeNormalized(state.game)
        val timeInfo = timeMs.map(ms => s" (${ms / 1000}s left)").getOrElse("")
        println(s"Game $gameId: Bot is thinking$timeInfo...")
        bot.chooseMove(state, timeMs, incrementMs).flatMap {
          case Left(err) =>
            println(s"Game $gameId: Bot failed to find move: $err")
            Future.unit
          case Right(move) =>
            val latest = currentState()
            if Fen.encodeNormalized(latest.game) != searchFen || latest.game.sideToMove != botColor then
              println(s"Game $gameId: Skipping stale bot move ${toUci(move)}; state changed while searching.")
              Future.unit
            else
              val uci = toUci(move)
              println(s"Game $gameId: Bot plays $uci")
              client.makeMove(tournamentId, gameId, uci).recover {
                case e => println(s"Game $gameId: Could not submit $uci: ${e.getMessage}")
              }
        }.andThen { case _ => thinking.set(false) }
    else
      println(s"Game $gameId: Waiting for ${state.game.sideToMove}. Bot is $botColor.")

  private def parseColor(value: String): Color =
    if value.equalsIgnoreCase("black") then Color.Black else Color.White

  private def timeFor(clock: Option[TournamentClock], color: Color): Option[Long] =
    clock.map { c =>
      val seconds = if color == Color.White then c.whiteTime else c.blackTime
      Math.max(0L, (seconds * 1000.0).toLong)
    }

  private def parseMoves(movesUci: String): Vector[Move] =
    movesUci.split("\\s+").filter(_.nonEmpty).flatMap(parseUciMove).toVector

  private def parseUciMove(uci: String): Option[Move] =
    if uci.length != 4 && uci.length != 5 then None
    else
      for
        from <- Pos.fromAlgebraic(uci.substring(0, 2)).toOption
        to <- Pos.fromAlgebraic(uci.substring(2, 4)).toOption
        promotion <- parsePromotion(uci.drop(4))
      yield Move(from, to, promotion)

  private def parsePromotion(suffix: String): Option[Option[PromotionRole]] =
    if suffix.isEmpty then Some(None)
    else PromotionRole.fromPromotionChar(suffix).toOption.map(Some(_))

  private def toUci(move: Move): String =
    val promotion = move.promotion.map {
      case PromotionRole.Queen  => "q"
      case PromotionRole.Rook   => "r"
      case PromotionRole.Bishop => "b"
      case PromotionRole.Knight => "n"
    }.getOrElse("")
    s"${move.from.toAlgebraic}${move.to.toAlgebraic}$promotion"
