package ch.tichess.view

import ch.tichess.model.*

final class GuiViewAdapter(initialGame: Game = Game.initial):
  private var currentGame: Game = initialGame
  private var selected: Option[Pos] = None
  private var legalTargets: Set[Pos] = Set.empty
  private var info: Option[String] = None
  private var moveLog: Vector[String] = Vector.empty

  def game: Game = currentGame
  def selectedPos: Option[Pos] = selected
  def legalTargetSquares: Set[Pos] = legalTargets
  def infoMessage: Option[String] = info
  def moveEntries: Vector[String] = moveLog
  def isGameOver: Boolean = currentGame.isCheckmate

  def statusText: String =
    if currentGame.isCheckmate then s"Schachmatt - ${colorLabel(currentGame.sideToMove.other)} gewinnt"
    else
      val turn = s"${colorLabel(currentGame.sideToMove)} to move"
      if currentGame.isInCheck then s"$turn | Schach" else turn

  def canSelect(pos: Pos): Boolean =
    !isGameOver && currentGame.board.pieceAt(pos).exists(_.color == currentGame.sideToMove)

  def handleSquareClick(pos: Pos): Unit =
    if isGameOver then ()
    else
      selected match
        case None =>
          if canSelect(pos) then select(pos)
        case Some(from) =>
          if from == pos then clearSelection()
          else if legalTargets.contains(pos) then attemptMove(from, pos)
          else if canSelect(pos) then select(pos)

  def setFen(fen: String): Unit =
    Fen.parse(fen.trim) match
      case Left(err) => info = Some(err)
      case Right(next) =>
        currentGame = next
        moveLog = Vector.empty
        clearSelection()
        info = Some("Position gesetzt.")

  private def select(pos: Pos): Unit =
    selected = Some(pos)
    legalTargets = legalMovesFrom(pos)
    info = None

  private def clearSelection(): Unit =
    selected = None
    legalTargets = Set.empty

  private def attemptMove(from: Pos, to: Pos): Unit =
    val mover = currentGame.sideToMove
    currentGame.applyMove(Move(from, to)) match
      case Left(err) => info = Some(err)
      case Right(next) =>
        currentGame = next
        moveLog = moveLog :+ formatMove(mover, Move(from, to))
        clearSelection()
        if next.isCheckmate then info = Some(s"Schachmatt - ${colorLabel(mover)} gewinnt")
        else if next.isInCheck then info = Some("Schach")
        else info = None

  private def legalMovesFrom(from: Pos): Set[Pos] =
    currentGame.legalMoves.collect { case Move(`from`, to) => to }.toSet

  private def colorLabel(color: Color): String = color match
    case Color.White => "White"
    case Color.Black => "Black"

  private def formatMove(mover: Color, move: Move): String =
    val moveNumber = moveLog.size / 2 + 1
    val side = mover match
      case Color.White => "W"
      case Color.Black => "B"
    s"$moveNumber. $side ${toAlg(move.from)}-${toAlg(move.to)}"

  private def toAlg(pos: Pos): String =
    s"${('a' + pos.file).toChar}${pos.rank + 1}"
