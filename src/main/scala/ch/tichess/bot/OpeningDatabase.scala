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
   * Bayesian-smoothed score for this move.
   *
   * A plain win-rate (wins/played) is unreliable for moves with few games —
   * a move played once and won gets 100%, beating e2e4 with 60% over 40k games.
   *
   * We fix this with Bayesian smoothing: we add a neutral 50% result prior
   * so that rare moves regress towards "playable but unproven" until enough
   * real data exists.
   *
   * Additionally, we score the lower bound of the expected result and multiply
   * by an uncapped confidence weight. That makes the book prefer robust main
   * lines over suspicious sidelines with a noisy high win rate.
   */
  def score: Double =
    val smoothing = 20.0
    val priorResult = 0.50

    val effectivePlayed = played + smoothing
    val result =
      (wins + 0.5 * draws + smoothing * priorResult) / effectivePlayed

    val uncertainty = Math.sqrt(result * (1.0 - result) / effectivePlayed)
    val conservativeResult = result - 1.15 * uncertainty
    val confidence = Math.log1p(played)

    conservativeResult * confidence

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
