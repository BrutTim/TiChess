package ch.tichess.analytics

import ch.tichess.controller.{AppState, Command}
import ch.tichess.model.{Color, Fen}

import java.util.UUID

final case class GameEvent(
    eventId: String,
    gameId: String,
    eventType: String,
    command: String,
    winner: Option[String],
    result: Option[String],
    moveCount: Long,
    timestamp: Long,
    fen: String
)

final case class PlayerStatistics(
    player: String,
    games: Long,
    victories: Long,
    draws: Long,
    losses: Long,
    score: Long,
    updatedAt: Long
)

object GameEventFactory:

  private final case class Outcome(winner: Option[String], result: String)

  private def colorName(color: Color): String = color match
    case Color.White => "White"
    case Color.Black => "Black"

  private def outcome(state: AppState, message: Option[String]): Option[Outcome] =
    if state.game.isCheckmate then
      Some(Outcome(Some(colorName(state.game.sideToMove.other)), "checkmate"))
    else
      state.resignedBy
        .map(loser => Outcome(Some(colorName(loser.other)), "resignation"))
        .orElse {
          if state.game.isDraw || state.drawAgreed then Some(Outcome(None, "draw"))
          else
            val normalized = message.getOrElse("").toLowerCase
            if normalized.contains("white wins") then Some(Outcome(Some("White"), "import"))
            else if normalized.contains("black wins") then Some(Outcome(Some("Black"), "import"))
            else if normalized.contains("remis (laut pgn)") then Some(Outcome(None, "draw"))
            else None
        }

  def create(
      gameId: String,
      input: String,
      before: AppState,
      after: AppState,
      message: Option[String],
      timestamp: Long = System.currentTimeMillis()
  ): Option[GameEvent] =
    val parsed = Command.parse(input).toOption
    val previousOutcome = outcome(before, None)
    val currentOutcome = outcome(after, message)

    val eventType =
      if currentOutcome.nonEmpty && currentOutcome != previousOutcome then Some("GameFinished")
      else
        parsed match
          case Some(Command.NewGame) => Some("GameStarted")
          case Some(Command.MoveCmd(_)) if after != before => Some("MovePlayed")
          case Some(Command.BotMove) if after != before    => Some("MovePlayed")
          case _                                           => None

    eventType.map { kind =>
      GameEvent(
        eventId = UUID.randomUUID().toString,
        gameId = gameId,
        eventType = kind,
        command = input.trim,
        winner = currentOutcome.flatMap(_.winner),
        result = currentOutcome.map(_.result),
        moveCount = after.moveHistory.size.toLong,
        timestamp = timestamp,
        fen = Fen.encode(after.game)
      )
    }
