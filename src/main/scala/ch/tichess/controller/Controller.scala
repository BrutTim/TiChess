package ch.tichess.controller

import ch.tichess.model.*

enum Command:
  case MoveCmd(move: Move)
  case SetFenCmd(fen: String)
  case Help
  case Quit

object Command:
  def parse(input: String): Either[String, Command] =
    val trimmed = input.trim
    if trimmed.isEmpty then Left("Empty input.")
    else
      val lower = trimmed.toLowerCase
      if lower.startsWith("fen") && lower.length > 3 && lower.charAt(3).isWhitespace then
        val fenStr = trimmed.substring(3).trim
        Right(Command.SetFenCmd(fenStr))
      else
        lower match
          case "fen" => Left("Expected a FEN after 'fen'.")
          case "q" | "quit" | "exit" => Right(Command.Quit)
          case "h" | "help"          => Right(Command.Help)
          case _ =>
            val parts = trimmed.split("\\s+").toList
            parts match
              case fromStr :: toStr :: Nil =>
                for
                  from <- Pos.fromAlgebraic(fromStr.toLowerCase)
                  to <- Pos.fromAlgebraic(toStr.toLowerCase)
                yield Command.MoveCmd(Move(from, to))
              case _ => Left("Expected a move like: e2 e4 (or 'help', 'quit', fen).")

final case class UpdateResult(game: Game, message: Option[String], quit: Boolean)

object Controller:
  def initial: Game = Game.initial

  private def colorLabel(c: Color): String = c match
    case Color.White => "White"
    case Color.Black => "Black"

  def update(game: Game, input: String): UpdateResult =
    Command.parse(input) match
      case Left(err) => UpdateResult(game, Some(err), quit = false)
      case Right(Command.Help) =>
        UpdateResult(game, Some(List(
                                      "- Zug eingeben: `e2 e4`",
                                      "- Hilfe anzeigen: `help`",
                                      "- Spiel beenden: `quit`",
                                      "- Position setzen (FEN, minimal): `fen <placement> <w|b>`",
                                      "- Beispiel: `fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w`"
                                    ).mkString("\n")), quit = false)
      case Right(Command.Quit) =>
        UpdateResult(game, Some("Bye."), quit = true)
      case Right(Command.MoveCmd(mv)) =>
        game.applyMove(mv) match
          case Left(err)     => UpdateResult(game, Some(err), quit = false)
          case Right(nextGm) =>
            if nextGm.isCheckmate then
              val winner = colorLabel(nextGm.sideToMove.other)
              UpdateResult(nextGm, Some(s"Checkmate. $winner wins."), quit = true)
            else UpdateResult(nextGm, None, quit = false)
      case Right(Command.SetFenCmd(fenStr)) =>
        ch.tichess.model.Fen.parse(fenStr) match
          case Left(err) => UpdateResult(game, Some(err), quit = false)
          case Right(newGame) =>
            if newGame.isCheckmate then
              val winner = colorLabel(newGame.sideToMove.other)
              UpdateResult(newGame, Some(s"Checkmate. $winner wins."), quit = true)
            else UpdateResult(newGame, Some("Position set."), quit = false)
