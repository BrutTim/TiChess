package ch.tichess.controller

import ch.tichess.model.*
import ch.tichess.controller.persistence.ChallengeRecord
import ch.tichess.services.ModelService

import scala.concurrent.{ExecutionContext, Future}

enum Command:
  case MoveCmd(move: Move)
  case LoadChallengeCmd(id: String)
  case RandomChallengeCmd
  case ImportFenCmd(fen: String)
  case ExportFenCmd
  case ImportPgnCmd(pgn: String)
  case ExportPgnCmd
  case SetParserCmd(parserId: String)
  case ShowParserCmd
  case Help
  case Quit
  case DrawOffer
  case DrawAccept
  case DrawDecline
  case Resign
  case NewGame

object Command:
  def parse(input: String): Either[String, Command] =
    val trimmed = input.trim
    if trimmed.isEmpty then Left("Empty input.")
    else
      val lower = trimmed.toLowerCase
      if lower == "fen export" then Right(Command.ExportFenCmd)
      else if lower.startsWith("fen import") && lower.length > 10 && lower.charAt(10).isWhitespace then
        Right(Command.ImportFenCmd(trimmed.substring(10).trim))
      else if lower.startsWith("fen") && lower.length > 3 && lower.charAt(3).isWhitespace then
        Right(Command.ImportFenCmd(trimmed.substring(3).trim))
      else if lower == "pgn export" then Right(Command.ExportPgnCmd)
      else if lower.startsWith("pgn import") && lower.length > 10 && lower.charAt(10).isWhitespace then
        Right(Command.ImportPgnCmd(trimmed.substring(10).trim))
      else if lower == "parser" then Right(Command.ShowParserCmd)
      else if lower.startsWith("parser") && lower.length > 6 && lower.charAt(6).isWhitespace then
        Right(Command.SetParserCmd(trimmed.substring(6).trim))
      else if lower.startsWith("challenge load") && lower.length > 14 && lower.charAt(14).isWhitespace then
        Right(Command.LoadChallengeCmd(trimmed.substring(14).trim))
      else if lower == "challenge random" then Right(Command.RandomChallengeCmd)
      else
        lower match
          case "fen" => Left("Expected a FEN after 'fen'.")
          case "pgn" => Left("Expected a PGN after 'pgn import' or use 'pgn export'.")
          case "q" | "quit" | "exit" => Right(Command.Quit)
          case "h" | "help"          => Right(Command.Help)
          case "draw"                => Right(Command.DrawOffer)
          case "accept"              => Right(Command.DrawAccept)
          case "decline" | "ablehnen" => Right(Command.DrawDecline)
          case "resign" | "aufgeben" => Right(Command.Resign)
          case "new" | "restart" | "neu" => Right(Command.NewGame)
          case "challenge" => Left("Expected a challenge command like: challenge random.")
          case _ =>
            val parts = trimmed.split("\\s+").toList
            parts match
              case fromStr :: toStr :: Nil =>
                for
                  from <- Pos.fromAlgebraic(fromStr.toLowerCase)
                  to <- Pos.fromAlgebraic(toStr.toLowerCase)
                yield Command.MoveCmd(Move(from, to))
              case fromStr :: toStr :: promoStr :: Nil =>
                for
                  from <- Pos.fromAlgebraic(fromStr.toLowerCase)
                  to <- Pos.fromAlgebraic(toStr.toLowerCase)
                  promotion <- PromotionRole.fromPromotionChar(promoStr)
                yield Command.MoveCmd(Move(from, to, Some(promotion)))
              case _ => Left("Expected a move like: e2 e4 (or 'help', 'quit', fen, pgn, parser, challenge).")

final case class ChallengeState(id: String, name: String, remainingMoves: Vector[Move])

final case class AppState(
    game: Game,
    parserChoice: ParserChoice = NotationParsers.default,
    startGame: Game = Game.initial,
    moveHistory: Vector[Move] = Vector.empty,
    drawOfferedBy: Option[Color] = None,
    resignedBy: Option[Color] = None,
    drawAgreed: Boolean = false,
    challengeMode: Option[ChallengeState] = None,
    challengeCompleted: Boolean = false
)

final case class UpdateResult(state: AppState, message: Option[String], quit: Boolean):
  def game: Game = state.game

object Controller:
  def initial: Game = Game.initial
  def initialState: AppState = AppState(Game.initial, startGame = Game.initial, moveHistory = Vector.empty, drawOfferedBy = None, resignedBy = None, drawAgreed = false)

  private def colorLabel(c: Color): String = c match
    case Color.White => "White"
    case Color.Black => "Black"

  private def parserSummary(choice: ParserChoice): String =
    s"Current parser: ${choice.id}. Available parsers: ${NotationParsers.ids.mkString(", ")}."

  private def parseMoveText(text: String): Either[String, Move] =
    Command.parse(text).flatMap {
      case Command.MoveCmd(move) => Right(move)
      case _                     => Left(s"Expected a move in challenge solution, got: $text")
    }

  private def parseChallengeMoves(moves: String): Either[String, Vector[Move]] =
    val parts = moves.split(",").map(_.trim).filter(_.nonEmpty).toVector
    if parts.isEmpty then Left("Challenge has no solution moves.")
    else
      parts.foldLeft(Right(Vector.empty): Either[String, Vector[Move]]) { (acc, part) =>
        for
          parsed <- acc
          move <- parseMoveText(part)
        yield parsed :+ move
      }

  private def challengeState(record: ChallengeRecord): Either[String, (Game, ChallengeState)] =
    for
      game <- Fen.parse(record.fen)
      moves <- parseChallengeMoves(record.moves)
    yield (game, ChallengeState(record.id, record.name, moves))

  private def applyChallengeMove(
      state: AppState,
      mv: Move,
      modelService: ModelService
  )(implicit ec: ExecutionContext): Future[UpdateResult] =
    state.challengeMode match
      case None =>
        applyRegularMove(state, mv, modelService)
      case Some(challenge) =>
        challenge.remainingMoves.headOption match
          case None =>
            Future.successful(UpdateResult(state.copy(challengeMode = None, challengeCompleted = true), Some("Challenge geloest!"), quit = false))
          case Some(expected) if expected != mv =>
            Future.successful(
              UpdateResult(
                state,
                Some("Falscher Zug, versuche es nochmal!"),
                quit = false
              )
            )
          case Some(_) =>
            modelService.applyMove(state.game, mv).flatMap {
              case Left(err) => Future.successful(UpdateResult(state, Some(err), quit = false))
              case Right(afterPlayerMove) =>
                val afterPlayerState = state.copy(
                  game = afterPlayerMove,
                  moveHistory = state.moveHistory :+ mv,
                  drawOfferedBy = None
                )
                val remainingAfterPlayer = challenge.remainingMoves.tail
                remainingAfterPlayer.headOption match
                  case None =>
                    val solvedState = afterPlayerState.copy(challengeMode = None, challengeCompleted = true)
                    Future.successful(UpdateResult(solvedState, Some("Challenge geloest!"), quit = false))
                  case Some(reply) =>
                    modelService.applyMove(afterPlayerMove, reply).map {
                      case Left(err) =>
                        UpdateResult(
                          afterPlayerState.copy(challengeMode = Some(challenge.copy(remainingMoves = remainingAfterPlayer))),
                          Some(s"Richtiger Zug, aber der Antwortzug konnte nicht ausgefuehrt werden: $err"),
                          quit = false
                        )
                      case Right(afterReply) =>
                        val stillRemaining = remainingAfterPlayer.tail
                        val nextChallenge = challenge.copy(remainingMoves = stillRemaining)
                        val nextState = afterPlayerState.copy(
                          game = afterReply,
                          moveHistory = afterPlayerState.moveHistory :+ reply,
                          challengeMode = Some(nextChallenge)
                        )
                        if stillRemaining.isEmpty then
                          UpdateResult(nextState.copy(challengeMode = None, challengeCompleted = true), Some("Challenge geloest!"), quit = false)
                        else
                          UpdateResult(nextState, Some("Richtig."), quit = false)
                    }
            }

  private def applyRegularMove(
      state: AppState,
      mv: Move,
      modelService: ModelService
  )(implicit ec: ExecutionContext): Future[UpdateResult] =
    modelService.applyMove(state.game, mv).map {
      case Left(err)     => UpdateResult(state, Some(err), quit = false)
      case Right(nextGm) =>
        val nextState = state.copy(game = nextGm, moveHistory = state.moveHistory :+ mv, drawOfferedBy = None)
        if nextGm.isCheckmate then
          val winner = colorLabel(nextGm.sideToMove.other)
          UpdateResult(nextState, Some(s"Checkmate. $winner wins."), quit = true)
        else if nextGm.isDraw then
          UpdateResult(nextState, Some(s"Draw (Stalemate)."), quit = true)
        else UpdateResult(nextState, None, quit = false)
    }

  def update(game: Game, input: String): UpdateResult =
    update(AppState(game), input)

  def updateAsync(
      state: AppState,
      input: String,
      modelService: ModelService,
      challengeLookup: String => Future[Option[ChallengeRecord]] = _ => Future.successful(None),
      randomChallenge: () => Future[Option[ChallengeRecord]] = () => Future.successful(None)
  )(implicit ec: ExecutionContext): Future[UpdateResult] =
    Command.parse(input) match
      case Left(err) => Future.successful(UpdateResult(state, Some(err), quit = false))
      case Right(Command.Help) =>
        Future.successful(UpdateResult(state, Some(List(
                                      "- Zug eingeben: `e2 e4`",
                                      "- Promotion: `e7 e8 q` (`q`, `r`, `b`, `n`)",
                                      "- Hilfe anzeigen: `help`",
                                      "- Spiel beenden: `quit`",
                                      "- Parser anzeigen: `parser`",
                                      "- Parser setzen: `parser <fastparse|combinators|regex>`",
                                      "- FEN importieren: `fen import <placement> <w|b>` oder `fen <placement> <w|b>`",
                                      "- FEN exportieren: `fen export`",
                                      "- PGN importieren: `pgn import <pgn>`",
                                      "- PGN exportieren: `pgn export`",
                                      "- Remis anbieten: `draw`",
                                      "- Remis annehmen: `accept`",
                                      "- Remis ablehnen: `decline`",
                                      "- Aufgeben: `resign`",
                                      "- Neues Spiel: `new`",
                                      "- Zufalls-Challenge starten: `challenge random`",
                                      "- Beispiel FEN: `fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w`"
                                    ).mkString("\n")), quit = false))
      case Right(Command.DrawOffer) =>
        if state.drawOfferedBy.isDefined then
          Future.successful(UpdateResult(state, Some("Es gibt bereits ein offenes Remis-Angebot."), quit = false))
        else
          val offerer = state.game.sideToMove
          // Flip side to move so the opponent must respond
          val nextGame = state.game.copy(sideToMove = offerer.other)
          val nextState = state.copy(game = nextGame, drawOfferedBy = Some(offerer))
          Future.successful(UpdateResult(nextState, Some(s"${colorLabel(offerer)} bietet Remis an. ${colorLabel(offerer.other)}: 'accept' oder 'decline' eingeben."), quit = false))
      case Right(Command.DrawAccept) =>
        state.drawOfferedBy match
          case Some(offerer) if offerer == state.game.sideToMove =>
            // The offerer is somehow still/again active – shouldn't normally happen
            Future.successful(UpdateResult(state, Some("Du kannst dein eigenes Remis-Angebot nicht annehmen."), quit = false))
          case Some(_) =>
            val nextState = state.copy(drawAgreed = true, drawOfferedBy = None)
            Future.successful(UpdateResult(nextState, Some("Spiel durch Remis-Übereinkunft beendet."), quit = true))
          case None =>
            Future.successful(UpdateResult(state, Some("Kein Remis-Angebot vorhanden."), quit = false))
      case Right(Command.DrawDecline) =>
        state.drawOfferedBy match
          case Some(offerer) =>
            // Flip back to the offerer's turn
            val nextGame = state.game.copy(sideToMove = offerer)
            val nextState = state.copy(game = nextGame, drawOfferedBy = None)
            Future.successful(UpdateResult(nextState, Some(s"Remis-Angebot abgelehnt. ${colorLabel(offerer)} ist wieder am Zug."), quit = false))
          case None =>
            Future.successful(UpdateResult(state, Some("Kein Remis-Angebot vorhanden."), quit = false))
      case Right(Command.Resign) =>
        val loser = state.game.sideToMove
        val winner = loser.other
        val nextState = state.copy(resignedBy = Some(loser))
        Future.successful(UpdateResult(nextState, Some(s"${colorLabel(loser)} gibt auf. ${colorLabel(winner)} gewinnt!"), quit = true))
      case Right(Command.NewGame) =>
        val nextState = initialState
        Future.successful(UpdateResult(nextState, Some("Neues Spiel gestartet."), quit = false))
      case Right(Command.Quit) =>
        Future.successful(UpdateResult(state, Some("Bye."), quit = true))
      case Right(Command.MoveCmd(_)) if state.drawOfferedBy.isDefined =>
        Future.successful(UpdateResult(state, Some("Remis-Angebot ausstehend. Bitte 'accept' oder 'decline' eingeben."), quit = false))
      case Right(Command.MoveCmd(mv)) =>
        applyChallengeMove(state, mv, modelService)
      case Right(Command.LoadChallengeCmd(id)) =>
        challengeLookup(id).map {
          case None =>
            UpdateResult(state, Some(s"Challenge nicht gefunden: $id"), quit = false)
          case Some(record) =>
            challengeState(record) match
              case Left(err) =>
                UpdateResult(state, Some(s"Challenge konnte nicht geladen werden: $err"), quit = false)
              case Right((game, challenge)) =>
                val nextState = state.copy(
                  game = game,
                  startGame = game,
                  moveHistory = Vector.empty,
                  drawOfferedBy = None,
                  resignedBy = None,
                  drawAgreed = false,
                  challengeMode = Some(challenge),
                  challengeCompleted = false
                )
                UpdateResult(nextState, Some("Challenge gestartet."), quit = false)
        }
      case Right(Command.RandomChallengeCmd) =>
        randomChallenge().map {
          case None =>
            UpdateResult(state, Some("Keine Challenge verfuegbar."), quit = false)
          case Some(record) =>
            challengeState(record) match
              case Left(err) =>
                UpdateResult(state, Some(s"Challenge konnte nicht geladen werden: $err"), quit = false)
              case Right((game, challenge)) =>
                val nextState = state.copy(
                  game = game,
                  startGame = game,
                  moveHistory = Vector.empty,
                  drawOfferedBy = None,
                  resignedBy = None,
                  drawAgreed = false,
                  challengeMode = Some(challenge),
                  challengeCompleted = false
                )
                UpdateResult(nextState, Some("Challenge gestartet."), quit = false)
        }
      case Right(Command.ImportFenCmd(fenStr)) =>
        state.parserChoice.fenParser.parse(fenStr) match
          case Left(err) => Future.successful(UpdateResult(state, Some(err), quit = false))
          case Right(newGame) =>
            val nextState = state.copy(game = newGame, startGame = newGame, moveHistory = Vector.empty, challengeMode = None, challengeCompleted = false)
            if newGame.isCheckmate then
              val winner = colorLabel(newGame.sideToMove.other)
              Future.successful(UpdateResult(nextState, Some(s"Checkmate. $winner wins."), quit = true))
            else if newGame.isDraw then
              Future.successful(UpdateResult(nextState, Some("Draw (Stalemate)."), quit = true))
            else Future.successful(UpdateResult(nextState, Some(s"Position set using ${state.parserChoice.id}."), quit = false))
      case Right(Command.ExportFenCmd) =>
        Future.successful(UpdateResult(state, Some(Fen.encode(state.game)), quit = false))
      case Right(Command.ImportPgnCmd(pgnStr)) =>
        Pgn.parse(pgnStr, state.parserChoice) match
          case Left(err) => Future.successful(UpdateResult(state, Some(err), quit = false))
          case Right(imported) =>
            val nextState = state.copy(
              game = imported.game,
              startGame = imported.startGame,
              moveHistory = imported.moves,
              challengeMode = None,
              challengeCompleted = false
            )
            if imported.game.isCheckmate then
              val winner = colorLabel(imported.game.sideToMove.other)
              Future.successful(UpdateResult(nextState, Some(s"Checkmate. $winner wins."), quit = true))
            else if imported.game.isDraw then
              Future.successful(UpdateResult(nextState, Some("Draw (Stalemate)."), quit = true))
            else imported.result match
              case "1/2-1/2" =>
                Future.successful(UpdateResult(nextState, Some("Remis (laut PGN)."), quit = true))
              case "1-0" =>
                Future.successful(UpdateResult(nextState, Some("White wins (laut PGN)."), quit = true))
              case "0-1" =>
                Future.successful(UpdateResult(nextState, Some("Black wins (laut PGN)."), quit = true))
              case _ =>
                Future.successful(UpdateResult(nextState, Some(s"PGN imported using ${state.parserChoice.id}."), quit = false))
      case Right(Command.ExportPgnCmd) =>
        Future.successful(UpdateResult(state, Some(Pgn.encode(state.startGame, state.moveHistory)), quit = false))
      case Right(Command.ShowParserCmd) =>
        Future.successful(UpdateResult(state, Some(parserSummary(state.parserChoice)), quit = false))
      case Right(Command.SetParserCmd(parserId)) =>
        NotationParsers.resolve(parserId) match
          case Left(err) => Future.successful(UpdateResult(state, Some(err), quit = false))
          case Right(choice) =>
            Future.successful(UpdateResult(state.copy(parserChoice = choice), Some(s"Parser set to ${choice.id}."), quit = false))

  def update(state: AppState, input: String): UpdateResult =
    import scala.concurrent.Await
    import scala.concurrent.duration.*
    implicit val ec: ExecutionContext = ExecutionContext.global
    Await.result(updateAsync(state, input, new ch.tichess.services.LocalModelService()), 5.seconds)
