package ch.tichess.bot.lichess

import akka.actor.typed.ActorSystem
import akka.stream.scaladsl.Sink
import ch.tichess.bot.ChessBot
import ch.tichess.controller.AppState
import ch.tichess.model.*

import scala.concurrent.{ExecutionContext, Future}

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
    var currentState = AppState(startGame)
    var whiteIncrementMs: Option[Long] = None
    var blackIncrementMs: Option[Long] = None

    client.streamGameEvents(gameId).runWith(Sink.foreach { event =>
      event.`type` match
        case "gameFull" =>
          event.state.foreach { st =>
            val time = if botColor == Color.White then st.wtime else st.btime
            whiteIncrementMs = st.winc.orElse(whiteIncrementMs)
            blackIncrementMs = st.binc.orElse(blackIncrementMs)
            val increment = if botColor == Color.White then whiteIncrementMs else blackIncrementMs
            startGame = parseInitialFen(event.initialFen).getOrElse(Game.initial)
            val fenInfo = event.initialFen.map(_ => ", custom initial FEN").getOrElse("")
            println(s"Game $gameId: full state received (${st.moves.split("\\s+").count(_.nonEmpty)} moves, status ${st.status}$fenInfo).")
            currentState = syncState(st.moves, startGame, gameId)
            checkTurnAndPlay(gameId, currentState, botColor, time, increment)
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
          currentState = syncState(moves, startGame, gameId)
          checkTurnAndPlay(gameId, currentState, botColor, time, increment)
        case _ => // chatLine etc.
    }).failed.foreach(e => println(s"Game loop $gameId failed: ${e.getMessage}"))

  private def parseInitialFen(initialFen: Option[String]): Option[Game] =
    initialFen.flatMap { fen =>
      Fen.parse(fen) match
        case Right(game) => Some(game)
        case Left(err) =>
          println(s"Could not parse Lichess initialFen '$fen': $err")
          None
    }

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

  private def checkTurnAndPlay(gameId: String, state: AppState, botColor: Color, timeMs: Option[Long], incrementMs: Option[Long]): Unit =
    if state.game.isCheckmate || state.game.isDraw then
      println(s"Game $gameId is over.")
    else if state.game.sideToMove == botColor then
      val incrementInfo = incrementMs.map(ms => s", +${ms / 1000.0}s").getOrElse("")
      val timeInfo = timeMs.map(ms => s" (${ms / 1000}s left$incrementInfo)").getOrElse("")
      println(s"Game $gameId: Bot is thinking$timeInfo...")
      bot.chooseMove(state, timeMs, incrementMs).flatMap {
        case Left(err) =>
          println(s"Game $gameId: Bot failed to find move: $err")
          Future.unit
        case Right(mv) =>
          val uci = toUci(mv)
          println(s"Game $gameId: Bot plays $uci")
          client.makeMove(gameId, uci)
      }.failed.foreach(e => println(s"Game $gameId: Error sending move: ${e.getMessage}"))
    else
      println(s"Game $gameId: Waiting for ${state.game.sideToMove}. Bot is $botColor.")

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
