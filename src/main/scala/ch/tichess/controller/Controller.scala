package ch.tichess.controller

import ch.tichess.model.*

enum Command:
  case MoveCmd(move: Move)
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
      else
        lower match
          case "fen" => Left("Expected a FEN after 'fen'.")
          case "pgn" => Left("Expected a PGN after 'pgn import' or use 'pgn export'.")
          case "q" | "quit" | "exit" => Right(Command.Quit)
          case "h" | "help"          => Right(Command.Help)
          case "draw"                => Right(Command.DrawOffer)
          case "accept"              => Right(Command.DrawAccept)
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
              case _ => Left("Expected a move like: e2 e4 (or 'help', 'quit', fen, pgn, parser).")

final case class AppState(
    game: Game,
    parserChoice: ParserChoice = NotationParsers.default,
    startGame: Game = Game.initial,
    moveHistory: Vector[Move] = Vector.empty,
    drawOfferedBy: Option[Color] = None
)

final case class UpdateResult(state: AppState, message: Option[String], quit: Boolean):
  def game: Game = state.game

object Controller:
  def initial: Game = Game.initial
  def initialState: AppState = AppState(Game.initial, startGame = Game.initial, moveHistory = Vector.empty, drawOfferedBy = None)

  private def colorLabel(c: Color): String = c match
    case Color.White => "White"
    case Color.Black => "Black"

  private def parserSummary(choice: ParserChoice): String =
    s"Current parser: ${choice.id}. Available parsers: ${NotationParsers.ids.mkString(", ")}."

  def update(game: Game, input: String): UpdateResult =
    update(AppState(game), input)

  import scala.concurrent.{Future, ExecutionContext}
  import ch.tichess.services.ModelService

  def updateAsync(state: AppState, input: String, modelService: ModelService)(implicit ec: ExecutionContext): Future[UpdateResult] =
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
                                      "- Beispiel FEN: `fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w`"
                                    ).mkString("\n")), quit = false))
      case Right(Command.DrawOffer) =>
        val offerer = state.game.sideToMove
        val nextState = state.copy(drawOfferedBy = Some(offerer))
        Future.successful(UpdateResult(nextState, Some(s"${colorLabel(offerer)} bietet Remis an. Zum Annehmen 'accept' eingeben."), quit = false))
      case Right(Command.DrawAccept) =>
        state.drawOfferedBy match
          case Some(_) =>
            Future.successful(UpdateResult(state, Some("Spiel durch Remis-Übereinkunft beendet."), quit = true))
          case None =>
            Future.successful(UpdateResult(state, Some("Kein Remis-Angebot vorhanden."), quit = false))
      case Right(Command.Quit) =>
        Future.successful(UpdateResult(state, Some("Bye."), quit = true))
      case Right(Command.MoveCmd(mv)) =>
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
      case Right(Command.ImportFenCmd(fenStr)) =>
        state.parserChoice.fenParser.parse(fenStr) match
          case Left(err) => Future.successful(UpdateResult(state, Some(err), quit = false))
          case Right(newGame) =>
            val nextState = state.copy(game = newGame, startGame = newGame, moveHistory = Vector.empty)
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
              moveHistory = imported.moves
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
