package ch.tichess.services

import ch.tichess.controller.AppState
import ch.tichess.model.{NotationParsers, PieceType, Pos}
import ch.tichess.view.{GuiViewAdapter, GuiViewState, StateResponse}

object StateResponseBuilder:
  private[services] def captureChar(kind: PieceType): String = kind match
    case PieceType.Pawn   => "♟"
    case PieceType.Knight => "♞"
    case PieceType.Bishop => "♝"
    case PieceType.Rook   => "♜"
    case PieceType.Queen  => "♛"
    case PieceType.King   => "♚"

  def fromAppState(appState: AppState): StateResponse =
    val guiState = GuiViewState(
      appState.game,
      startGame = appState.startGame,
      drawAgreed = appState.drawAgreed,
      resignedBy = appState.resignedBy,
      selectedParserId = appState.parserChoice.id,
      moveHistory = appState.moveHistory,
      moveEntries = GuiViewAdapter.buildMoveEntries(appState.startGame, appState.moveHistory)
    )

    val advantage = guiState.materialAdvantage

    def capDisplay(pieces: List[PieceType], showAdvantage: Boolean): String =
      val symbols = pieces.map(captureChar).mkString
      val advantageText = if showAdvantage then s" +${Math.abs(advantage)}" else ""
      if symbols.isEmpty && !showAdvantage then "" else s"$symbols$advantageText"

    val legalMoves =
      (for
        rank <- 0 to 7
        file <- 0 to 7
        pos = Pos(file, rank)
      yield pos.toAlgebraic -> appState.game.legalMoves.filter(_.from == pos).map(_.to.toAlgebraic).toList)
        .toMap
        .filter(_._2.nonEmpty)

    val lastMove = appState.moveHistory.lastOption

    val statusText =
      if appState.challengeCompleted then "Challenge geloest."
      else
        appState.challengeMode match
          case Some(_) => "Challenge aktiv."
          case None    => guiState.statusText

    StateResponse(
      fen = ch.tichess.model.Fen.encode(appState.game),
      statusText = statusText,
      isGameOver = guiState.isGameOver || appState.challengeCompleted,
      drawOffered = appState.drawOfferedBy.nonEmpty,
      whiteCaptured = capDisplay(guiState.capturedByWhite, advantage > 0),
      blackCaptured = capDisplay(guiState.capturedByBlack, advantage < 0),
      moveList = guiState.moveEntries.toList,
      legalMoves = legalMoves,
      lastMoveFrom = lastMove.map(_.from.toAlgebraic),
      lastMoveTo = lastMove.map(_.to.toAlgebraic),
      currentParser = appState.parserChoice.id,
      availableParsers = NotationParsers.ids.toList,
      challengeHintFrom = appState.challengeMode.flatMap(_.remainingMoves.headOption.map(_.from.toAlgebraic)),
      challengeSideToMove = appState.challengeMode.map(_ =>
        appState.game.sideToMove match
          case ch.tichess.model.Color.White => "Weiss"
          case ch.tichess.model.Color.Black => "Schwarz"
      ),
      activeBot = appState.activeBot.map {
        case ch.tichess.model.Color.White => "w"
        case ch.tichess.model.Color.Black => "b"
      }
    )
