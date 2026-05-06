package ch.tichess.bot

import ch.tichess.model.{Color, Fen, Game, Move, Pgn}

import scala.concurrent.{ExecutionContext, Future}

/**
 * An in-memory implementation of the OpeningDatabase that can be seeded
 * from PGN files (offline solution).
 */
class PgnOpeningDatabase(implicit ec: ExecutionContext) extends OpeningDatabase:
  // Map[NormalizedFen -> Map[Move -> Stats]]
  private var db = Map.empty[String, Map[Move, OpeningMoveStats]]

  override def getMoves(normalizedFen: String): Future[List[OpeningMoveStats]] = Future.successful {
    db.getOrElse(normalizedFen, Map.empty).values.toList
  }

  override def recordOutcome(normalizedFen: String, move: Move, result: Int): Future[Unit] = Future.successful {
    val stateDb = db.getOrElse(normalizedFen, Map.empty)
    val stats = stateDb.getOrElse(move, OpeningMoveStats(move, 0, 0, 0, 0))
    
    val newStats = result match
      case 1  => stats.copy(played = stats.played + 1, wins = stats.wins + 1)
      case 0  => stats.copy(played = stats.played + 1, draws = stats.draws + 1)
      case -1 => stats.copy(played = stats.played + 1, losses = stats.losses + 1)
      case _  => stats.copy(played = stats.played + 1) // Just played
      
    db = db.updated(normalizedFen, stateDb.updated(move, newStats))
  }

  /**
   * Loads multiple games from a PGN string. Assumes standard PGN format where
   * games start with an [Event "..."] tag.
   */
  def loadFromPgnString(pgnData: String, maxMovesPerGame: Int = 20): Int =
    // Normalize line endings (Windows \r\n → \n)
    val normalized = pgnData.replace("\r\n", "\n").replace("\r", "\n")
    // Split by [Event to separate multiple games in a single file
    val rawGames = normalized.split("(?=\\[Event )").map(_.trim).filter(_.nonEmpty)
    var loadedCount = 0
    var errorCount  = 0

    for gameStr <- rawGames do
      Pgn.parse(gameStr) match
        case Right(imported) =>
          val whiteResult = imported.result match
            case "1-0" => 1
            case "0-1" => -1
            case "1/2-1/2" => 0
            case _ => 0 // Unclear, count as draw/played

          var currentGame = imported.startGame
          // Only process the opening phase to save memory and focus on actual openings
          val openingMoves = imported.moves.take(maxMovesPerGame)
          
          for move <- openingMoves do
            val fen = Fen.encodeNormalized(currentGame)
            val playerResult = if currentGame.sideToMove == Color.White then whiteResult else -whiteResult
            
            // Record synchronously for the initial load
            val stateDb = db.getOrElse(fen, Map.empty)
            val stats = stateDb.getOrElse(move, OpeningMoveStats(move, 0, 0, 0, 0))
            val newStats =
              if playerResult > 0 then stats.copy(played = stats.played + 1, wins = stats.wins + 1)
              else if playerResult < 0 then stats.copy(played = stats.played + 1, losses = stats.losses + 1)
              else stats.copy(played = stats.played + 1, draws = stats.draws + 1)
            
            db = db.updated(fen, stateDb.updated(move, newStats))

            currentGame = currentGame.applyMove(move).toOption.getOrElse(currentGame)
          
          loadedCount += 1
        case Left(_) =>
          errorCount += 1 // Count but silently ignore parse failures

    if errorCount > 0 then
      println(s"[OpeningDB] Warning: $errorCount games could not be parsed and were skipped.")
    loadedCount

  /**
   * Helper to inspect the database size.
   */
  def positionCount: Int = db.size
