package ch.tichess.view

import ch.tichess.model.*

final case class GuiViewState(
    game: Game,
    selectedPos: Option[Pos] = None,
    legalTargetSquares: Set[Pos] = Set.empty,
    infoMessage: Option[String] = None,
    moveEntries: Vector[String] = Vector.empty
):
  def isGameOver: Boolean = game.isCheckmate

  def statusText: String =
    if game.isCheckmate then s"Schachmatt - ${GuiViewAdapter.colorLabel(game.sideToMove.other)} gewinnt"
    else
      val turn = s"${GuiViewAdapter.colorLabel(game.sideToMove)} to move"
      if game.isInCheck then s"$turn | Schach" else turn

object GuiViewState:
  val initial: GuiViewState = GuiViewState(Game.initial)

final class GuiViewAdapter(initialGame: Game = Game.initial):
  def initialState: GuiViewState = GuiViewState(initialGame)

object GuiViewAdapter:
  def canSelect(state: GuiViewState, pos: Pos): Boolean =
    !state.isGameOver && state.game.board.pieceAt(pos).exists(_.color == state.game.sideToMove)

  def handleSquareClick(state: GuiViewState, pos: Pos): GuiViewState =
    if state.isGameOver then state
    else
      state.selectedPos match
        case None =>
          if canSelect(state, pos) then select(state, pos) else state
        case Some(from) =>
          if from == pos then clearSelection(state)
          else if state.legalTargetSquares.contains(pos) then attemptMove(state, from, pos)
          else if canSelect(state, pos) then select(state, pos)
          else state

  def setFen(state: GuiViewState, fen: String): GuiViewState =
    Fen.parse(fen.trim) match
      case Left(err) => state.copy(infoMessage = Some(err))
      case Right(next) =>
        clearSelection(
          state.copy(
            game = next,
            moveEntries = Vector.empty,
            infoMessage = Some("Position gesetzt.")
          )
        )

  private def select(state: GuiViewState, pos: Pos): GuiViewState =
    state.copy(
      selectedPos = Some(pos),
      legalTargetSquares = legalMovesFrom(state.game, pos),
      infoMessage = None
    )

  private def clearSelection(state: GuiViewState): GuiViewState =
    state.copy(selectedPos = None, legalTargetSquares = Set.empty)

  private def attemptMove(state: GuiViewState, from: Pos, to: Pos): GuiViewState =
    val mover = state.game.sideToMove
    state.game.applyMove(Move(from, to)) match
      case Left(err) => state.copy(infoMessage = Some(err))
      case Right(next) =>
        val updated = clearSelection(
          state.copy(
            game = next,
            moveEntries = state.moveEntries :+ formatMove(state.moveEntries, mover, Move(from, to))
          )
        )
        if next.isCheckmate then updated.copy(infoMessage = Some(s"Schachmatt - ${colorLabel(mover)} gewinnt"))
        else if next.isInCheck then updated.copy(infoMessage = Some("Schach"))
        else updated.copy(infoMessage = None)

  private def legalMovesFrom(game: Game, from: Pos): Set[Pos] =
    game.legalMoves.collect { case Move(`from`, to) => to }.toSet

  private def formatMove(moveLog: Vector[String], mover: Color, move: Move): String =
    val moveNumber = moveLog.size / 2 + 1
    val side = mover match
      case Color.White => "W"
      case Color.Black => "B"
    s"$moveNumber. $side ${toAlg(move.from)}-${toAlg(move.to)}"

  private def toAlg(pos: Pos): String =
    s"${('a' + pos.file).toChar}${pos.rank + 1}"

  def colorLabel(color: Color): String = color match
    case Color.White => "White"
    case Color.Black => "Black"
