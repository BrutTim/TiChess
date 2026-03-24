package ch.tichess.controller

import ch.tichess.model.*

enum Command:
  case MoveCmd(move: Move)
  case Help
  case Quit

object Command:
  def parse(input: String): Either[String, Command] =
    val trimmed = input.trim
    if trimmed.isEmpty then Left("Empty input.")
    else
      val lower = trimmed.toLowerCase
      lower match
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
            case _ => Left("Expected a move like: e2 e4 (or 'help', 'quit').")

final case class UpdateResult(game: Game, message: Option[String], quit: Boolean)

object Controller:
  def initial: Game = Game.initial

  def update(game: Game, input: String): UpdateResult =
    Command.parse(input) match
      case Left(err) => UpdateResult(game, Some(err), quit = false)
      case Right(Command.Help) =>
        UpdateResult(game, Some("Enter moves like 'e2 e4'. Commands: help, quit."), quit = false)
      case Right(Command.Quit) =>
        UpdateResult(game, Some("Bye."), quit = true)
      case Right(Command.MoveCmd(mv)) =>
        game.applyMove(mv) match
          case Left(err)     => UpdateResult(game, Some(err), quit = false)
          case Right(nextGm) => UpdateResult(nextGm, None, quit = false)
