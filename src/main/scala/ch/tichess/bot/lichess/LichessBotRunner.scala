package ch.tichess.bot.lichess

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.Sink
import ch.tichess.bot.ChessBot
import ch.tichess.controller.AppState
import ch.tichess.model.*

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.{blocking, ExecutionContext, Future}
import scala.concurrent.duration.*

class LichessBotRunner(client: LichessClient, bot: ChessBot)(implicit system: ActorSystem[?], ec: ExecutionContext):

  def start(): Unit =
    println(">>> Starting Lichess Bot Runner...")
    client.streamEvents().runWith(Sink.foreach { event =>
      event.`type` match
        case "challenge" =>
          event.challenge.foreach { c =>
            println(s"Received challenge from Lichess: ${c.id}")
            // Accept all challenges automatically (as requested)
            client.acceptChallenge(c.id).onComplete {
              case scala.util.Success(_) => println(s"Accepted challenge ${c.id}")
              case scala.util.Failure(e) => println(s"Could not accept challenge ${c.id}: ${e.getMessage}")
            }
          }
        case "gameStart" =>
          event.game.foreach { g =>
            val botColorStr = g.color.getOrElse("white")
            val botColor = if botColorStr == "white" then Color.White else Color.Black
            println(s"Game started: ${g.gameId}. Bot plays as $botColor")
            startGameLoop(g.gameId, botColor)
          }
        case t => // ignore
    }).failed.foreach(e => println(s"Lichess Event Stream failed: ${e.getMessage}"))

  private def startGameLoop(gameId: String, botColor: Color): Unit =
    var startGame = Game.initial
    val currentState = AtomicReference[AppState](AppState(startGame))
    val thinking = AtomicBoolean(false)
    var whiteIncrementMs: Option[Long] = None
    var blackIncrementMs: Option[Long] = None
    var lastPonderFen: Option[String] = None
    var lastPredictedReplies: List[String] = Nil

    client.streamGameEvents(gameId).runWith(Sink.foreach { event =>
      event.`type` match
        case "gameFull" =>
          event.state.foreach { st =>
            val time = if botColor == Color.White then st.wtime else st.btime
            whiteIncrementMs = st.winc.orElse(whiteIncrementMs)
            blackIncrementMs = st.binc.orElse(blackIncrementMs)
            val increment = if botColor == Color.White then whiteIncrementMs else blackIncrementMs
            startGame = parseInitialFen(event.initialFen).getOrElse(Game.initial)
            val fenInfo = event.initialFen.filterNot(isStartPosition).map(_ => ", custom initial FEN").getOrElse("")
            println(s"Game $gameId: full state received (${st.moves.split("\\s+").count(_.nonEmpty)} moves, status ${st.status}$fenInfo).")
            val syncedState = syncState(st.moves, startGame, gameId)
            currentState.set(syncedState)
            reportPonderResult(gameId, syncedState, botColor, latestUci(st.moves), () => lastPredictedReplies, value => lastPredictedReplies = value)
            checkTurnAndPlay(gameId, syncedState, botColor, time, increment, () => lastPonderFen, fen => lastPonderFen = fen, value => lastPredictedReplies = value, thinking, () => currentState.get())
          }
        case "gameState" =>
          val time = if botColor == Color.White then event.wtime else event.btime
          event.state.foreach { st =>
            whiteIncrementMs = st.winc.orElse(whiteIncrementMs)
            blackIncrementMs = st.binc.orElse(blackIncrementMs)
          }
          val increment = if botColor == Color.White then whiteIncrementMs else blackIncrementMs
          val moves = event.moves.getOrElse("")
          println(s"Game $gameId: state update (${moves.split("\\s+").count(_.nonEmpty)} moves, status ${event.status.getOrElse("?")}).")
          val syncedState = syncState(moves, startGame, gameId)
          currentState.set(syncedState)
          reportPonderResult(gameId, syncedState, botColor, latestUci(moves), () => lastPredictedReplies, value => lastPredictedReplies = value)
          checkTurnAndPlay(gameId, syncedState, botColor, time, increment, () => lastPonderFen, fen => lastPonderFen = fen, value => lastPredictedReplies = value, thinking, () => currentState.get())
        case _ => // chatLine etc.
    }).failed.foreach(e => println(s"Game loop $gameId failed: ${e.getMessage}"))

  private def parseInitialFen(initialFen: Option[String]): Option[Game] =
    initialFen.flatMap { fen =>
      if isStartPosition(fen) then Some(Game.initial)
      else Fen.parse(fen) match
        case Right(game) => Some(game)
        case Left(err) =>
          println(s"Could not parse Lichess initialFen '$fen': $err")
          None
    }

  private def isStartPosition(fen: String): Boolean =
    fen.trim == "startpos"

  private def syncState(movesUci: String, startGame: Game, gameId: String): AppState =
    if movesUci.trim.isEmpty then AppState(startGame, startGame = startGame, moveHistory = Vector.empty)
    else
      val ucis = movesUci.split("\\s+").filter(_.nonEmpty).toList
      val (finalGame, moves) = ucis.foldLeft((startGame, Vector.empty[Move])) { case ((game, history), uci) =>
        parseUciMove(uci) match
          case Some(mv) =>
            game.applyMove(mv) match
              case Right(next) => (next, history :+ mv)
              case Left(err) =>
                println(s"Game $gameId: Could not apply Lichess move $uci on local board: $err")
                (game, history)
          case None =>
            println(s"Game $gameId: Could not parse Lichess move $uci.")
            (game, history)
      }
      AppState(finalGame, startGame = startGame, moveHistory = moves)

  private def checkTurnAndPlay(
      gameId: String,
      state: AppState,
      botColor: Color,
      timeMs: Option[Long],
      incrementMs: Option[Long],
      lastPonderFen: () => Option[String],
      setLastPonderFen: Option[String] => Unit,
      setLastPredictedReplies: List[String] => Unit,
      thinking: AtomicBoolean,
      currentState: () => AppState
  ): Unit =
    if state.game.isCheckmate || state.game.isDraw then
      println(s"Game $gameId is over.")
    else if state.game.sideToMove == botColor then
      if !thinking.compareAndSet(false, true) then
        println(s"Game $gameId: Bot is already thinking; skipping duplicate turn update.")
      else
        setLastPonderFen(None)
        val searchFen = Fen.encodeNormalized(state.game)
        val incrementInfo = incrementMs.map(ms => s", +${ms / 1000.0}s").getOrElse("")
        val timeInfo = timeMs.map(ms => s" (${ms / 1000}s left$incrementInfo)").getOrElse("")
        println(s"Game $gameId: Bot is thinking$timeInfo...")
        bot.chooseMove(state, timeMs, incrementMs).flatMap {
          case Left(err) =>
            println(s"Game $gameId: Bot failed to find move: $err")
            Future.unit
          case Right(mv) =>
            val latest = currentState()
            val latestFen = Fen.encodeNormalized(latest.game)
            if latest.game.isCheckmate || latest.game.isDraw || latest.game.sideToMove != botColor || latestFen != searchFen then
              println(s"Game $gameId: Skipping stale bot move ${toUci(mv)}; turn/state changed while searching.")
              Future.unit
            else
              val uci = toUci(mv)
              println(s"Game $gameId: Bot plays $uci")
              sendMoveWithRetry(gameId, uci, searchFen, botColor, currentState)
        }.andThen { case _ => thinking.set(false) }
          .failed.foreach(e => println(s"Game $gameId: Error sending move: ${e.getMessage}"))
    else
      println(s"Game $gameId: Waiting for ${state.game.sideToMove}. Bot is $botColor.")
      startPonderIfUseful(gameId, state, timeMs, lastPonderFen, setLastPonderFen, setLastPredictedReplies)

  private def startPonderIfUseful(
      gameId: String,
      state: AppState,
      timeMs: Option[Long],
      lastPonderFen: () => Option[String],
      setLastPonderFen: Option[String] => Unit,
      setLastPredictedReplies: List[String] => Unit
  ): Unit =
    val fen = Fen.encodeNormalized(state.game)
    val budget = ponderBudgetMs(timeMs)
    if budget > 0L && lastPonderFen().forall(_ != fen) then
      setLastPonderFen(Some(fen))
      bot.predictedReplies(state).take(3) match
        case predicted :: rest =>
          val predictedUci = toUci(predicted)
          state.game.applyMove(predicted) match
            case Right(predictedGame) =>
              val predictedState = state.copy(game = predictedGame, moveHistory = state.moveHistory :+ predicted)
              val predictedUcis = (predicted :: rest).map(toUci)
              setLastPredictedReplies(predictedUcis)
              val topInfo = if predictedUcis.size > 1 then s" (top ${predictedUcis.mkString(",")})" else ""
              println(s"Game $gameId: Ponder predicted $predictedUci$topInfo for up to ${budget}ms.")
              bot.ponder(predictedState, budget).failed.foreach(e => println(s"Game $gameId: Ponder failed: ${e.getMessage}"))
            case Left(_) =>
              setLastPredictedReplies(Nil)
              println(s"Game $gameId: Ponder warmup for up to ${budget}ms.")
              bot.ponder(state, budget).failed.foreach(e => println(s"Game $gameId: Ponder failed: ${e.getMessage}"))
        case Nil =>
          setLastPredictedReplies(Nil)
          println(s"Game $gameId: Ponder warmup for up to ${budget}ms.")
          bot.ponder(state, budget).failed.foreach(e => println(s"Game $gameId: Ponder failed: ${e.getMessage}"))

  private def reportPonderResult(
      gameId: String,
      state: AppState,
      botColor: Color,
      latestMoveUci: Option[String],
      lastPredictedReplies: () => List[String],
      setLastPredictedReplies: List[String] => Unit
  ): Unit =
    if state.game.sideToMove == botColor then
      val predictions = lastPredictedReplies()
      if predictions.nonEmpty then
        val top = predictions.head
        latestMoveUci match
          case Some(actual) if actual == top =>
            println(s"Game $gameId: Ponder hit top1 ($actual).")
          case Some(actual) if predictions.contains(actual) =>
            val rank = predictions.indexOf(actual) + 1
            println(s"Game $gameId: Ponder near-hit top$rank ($actual; top1 $top).")
          case Some(actual) =>
            println(s"Game $gameId: Ponder miss (top ${predictions.mkString(",")}, got $actual).")
          case None =>
            println(s"Game $gameId: Ponder miss (top ${predictions.mkString(",")}, no latest move).")
        setLastPredictedReplies(Nil)

  private def latestUci(movesUci: String): Option[String] =
    movesUci.split("\\s+").filter(_.nonEmpty).lastOption

  private def sendMoveWithRetry(
      gameId: String,
      uci: String,
      searchFen: String,
      botColor: Color,
      currentState: () => AppState
  ): Future[Unit] =
    val retryDelays = List(250.millis, 800.millis)

    def stillSameTurn: Boolean =
      val latest = currentState()
      Fen.encodeNormalized(latest.game) == searchFen &&
        latest.game.sideToMove == botColor &&
        !latest.game.isCheckmate &&
        !latest.game.isDraw

    def attempt(delays: List[FiniteDuration]): Future[Unit] =
      client.makeMove(gameId, uci).recoverWith {
        case e if noLongerOurTurn(e) =>
          println(s"Game $gameId: Move $uci no longer accepted by Lichess; state likely changed or move was already processed.")
          Future.unit
        case e if transientSendError(e) && delays.nonEmpty =>
          if stillSameTurn then
            val delay = delays.head
            println(s"Game $gameId: transient send error for $uci (${e.getMessage}); retrying in ${delay.toMillis}ms.")
            Future(blocking(Thread.sleep(delay.toMillis))).flatMap(_ =>
              if stillSameTurn then attempt(delays.tail)
              else
                println(s"Game $gameId: Skipping retry for $uci; state changed after transient send error.")
                Future.unit
            )
          else
            println(s"Game $gameId: Skipping retry for $uci; state changed after transient send error.")
            Future.unit
      }

    attempt(retryDelays)

  private def transientSendError(error: Throwable): Boolean =
    val message = Option(error.getMessage).getOrElse("").toLowerCase
    message.contains("closed the connection") ||
      message.contains("connection reset") ||
      message.contains("connection refused") ||
      message.contains("premature") ||
      message.contains("timeout") ||
      message.contains("temporarily")

  private def noLongerOurTurn(error: Throwable): Boolean =
    val message = Option(error.getMessage).getOrElse("").toLowerCase
    message.contains("not your turn") || message.contains("game already over")

  private def ponderBudgetMs(timeMs: Option[Long]): Long =
    timeMs match
      case Some(ms) if ms >= 24L * 60L * 60L * 1000L => 3000L
      case Some(ms) =>
        val budget = ms / 20L
        if budget < 300L then 0L else Math.min(5000L, budget)
      case None => 3000L

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
