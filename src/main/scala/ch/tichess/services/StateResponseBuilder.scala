package ch.tichess.services

import ch.tichess.controller.AppState
import ch.tichess.model.{NotationParsers, PieceType, Pos}
import ch.tichess.view.{GuiViewAdapter, GuiViewState, StateResponse}

object StateResponseBuilder:
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

    def capChar(kind: PieceType): String = kind match
      case PieceType.Pawn   => "♟"
      case PieceType.Knight => "♞"
      case PieceType.Bishop => "♝"
      case PieceType.Rook   => "♜"
      case PieceType.Queen  => "♛"
      case PieceType.King   => "♚"

    def capDisplay(pieces: List[PieceType], showAdvantage: Boolean): String =
      val symbols = pieces.map(capChar).mkString
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

    StateResponse(
      fen = ch.tichess.model.Fen.encode(appState.game),
      statusText = guiState.statusText,
      isGameOver = guiState.isGameOver,
      drawOffered = appState.drawOfferedBy.nonEmpty,
      whiteCaptured = capDisplay(guiState.capturedByWhite, advantage > 0),
      blackCaptured = capDisplay(guiState.capturedByBlack, advantage < 0),
      moveList = guiState.moveEntries.toList,
      legalMoves = legalMoves,
      currentParser = appState.parserChoice.id,
      availableParsers = NotationParsers.ids.toList
    )
