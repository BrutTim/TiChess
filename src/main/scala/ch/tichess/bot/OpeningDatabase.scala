package ch.tichess.bot

import ch.tichess.model.Move

import scala.concurrent.Future

/**
 * Statistics for a specific move played in a specific position.
 * This forms the basis of the bot's learning capability in the opening.
 */
final case class OpeningMoveStats(
    move: Move,
    played: Int,
    wins: Int,
    draws: Int,
    losses: Int
):
  /**
   * Simple score for a move based on win/draw/loss ratio and frequency.
   * A move played more often with better results gets a higher score.
   */
  def score: Double =
    if played == 0 then 0.0
    else
      // Weight wins higher than draws, losses negatively.
      val winRate = wins.toDouble / played
      val drawRate = draws.toDouble / played
      winRate + (0.5 * drawRate)

/**
 * Interface for the Opening Book / Database.
 * Allows the bot to look up known good moves in early game phases,
 * and allows recording game outcomes to improve future play.
 */
trait OpeningDatabase:
  /**
   * Queries the database for known moves in a specific position.
   *
   * @param normalizedFen A FEN string that excludes move counters to allow
   *                      matching the same position reached by transpositions.
   */
  def getMoves(normalizedFen: String): Future[List[OpeningMoveStats]]

  /**
   * Records the outcome of a game to update the statistics.
   * 
   * @param normalizedFen The position where the move was played.
   * @param move The move that was played.
   * @param result 1 for win, 0 for draw, -1 for loss (from the perspective of the player making the move).
   */
  def recordOutcome(normalizedFen: String, move: Move, result: Int): Future[Unit]
