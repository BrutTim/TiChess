package ch.tichess.view

import ch.tichess.model.*

final case class PendingPromotion(from: Pos, to: Pos, color: Color)

final case class GuiViewState(
    game: Game,
    selectedPos: Option[Pos] = None,
    legalTargetSquares: Set[Pos] = Set.empty,
    infoMessage: Option[String] = None,
    moveEntries: Vector[String] = Vector.empty,
    pendingPromotion: Option[PendingPromotion] = None
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
    if state.isGameOver || state.pendingPromotion.nonEmpty then state
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
            infoMessage = Some("Position gesetzt."),
            pendingPromotion = None
          )
        )

  def choosePromotion(state: GuiViewState, role: PromotionRole): GuiViewState =
    state.pendingPromotion match
      case None => state
      case Some(pending) =>
        applyMove(
          state,
          Move(pending.from, pending.to, Some(role)),
          Some(s"Promotion wählen: ${promotionRoleLabel(role)}")
        )

  def cancelPromotion(state: GuiViewState): GuiViewState =
    state.copy(pendingPromotion = None, infoMessage = None)

  private def select(state: GuiViewState, pos: Pos): GuiViewState =
    state.copy(
      selectedPos = Some(pos),
      legalTargetSquares = legalMovesFrom(state.game, pos),
      infoMessage = None
    )

  private def clearSelection(state: GuiViewState): GuiViewState =
    state.copy(selectedPos = None, legalTargetSquares = Set.empty, pendingPromotion = None)

  private def attemptMove(state: GuiViewState, from: Pos, to: Pos): GuiViewState =
    state.game.board.pieceAt(from) match
      case Some(Piece(color, PieceType.Pawn)) if promotionRank(color, to.rank) =>
        state.copy(
          pendingPromotion = Some(PendingPromotion(from, to, color)),
          infoMessage = Some("Promotion wählen: Dame, Turm, Läufer oder Springer.")
        )
      case _ =>
        applyMove(state, Move(from, to), None)

  private def applyMove(state: GuiViewState, move: Move, pendingInfo: Option[String]): GuiViewState =
    val mover = state.game.sideToMove
    val movingPiece = state.game.board.pieceAt(move.from)
    state.game.applyMove(move) match
      case Left(err) => state.copy(infoMessage = Some(err))
      case Right(next) =>
        val updated = clearSelection(
          state.copy(
            game = next,
            moveEntries = state.moveEntries :+ formatMove(state.moveEntries, mover, move, movingPiece, next.board),
            infoMessage = pendingInfo.orElse(state.infoMessage)
          )
        )
        if next.isCheckmate then updated.copy(infoMessage = Some(s"Schachmatt - ${colorLabel(mover)} gewinnt"))
        else if next.isInCheck then updated.copy(infoMessage = Some("Schach"))
        else updated.copy(infoMessage = None)

  private def legalMovesFrom(game: Game, from: Pos): Set[Pos] =
    game.legalMoves.collect { case Move(`from`, to, _) => to }.toSet

  private def formatMove(
      moveLog: Vector[String],
      mover: Color,
      move: Move,
      movingPiece: Option[Piece],
      boardAfter: Board
  ): String =
    val moveNumber = moveLog.size / 2 + 1
    val side = mover match
      case Color.White => "W"
      case Color.Black => "B"
    val promotionSuffix =
      (movingPiece, boardAfter.pieceAt(move.to)) match
        case (Some(Piece(_, PieceType.Pawn)), Some(Piece(_, kind))) if move.to.rank == 0 || move.to.rank == 7 =>
          move.promotion.fold("") {
            case PromotionRole.Queen  => "=Q"
            case PromotionRole.Rook   => "=R"
            case PromotionRole.Bishop => "=B"
            case PromotionRole.Knight => "=N"
          }
        case _ => ""
    s"$moveNumber. $side ${toAlg(move.from)}-${toAlg(move.to)}$promotionSuffix"

  private def toAlg(pos: Pos): String =
    s"${('a' + pos.file).toChar}${pos.rank + 1}"

  private def promotionRank(color: Color, rank: Int): Boolean =
    (color == Color.White && rank == 7) || (color == Color.Black && rank == 0)

  private def promotionRoleLabel(kind: PromotionRole): String = kind match
    case PromotionRole.Queen  => "Dame"
    case PromotionRole.Rook   => "Turm"
    case PromotionRole.Bishop => "Läufer"
    case PromotionRole.Knight => "Springer"

  def colorLabel(color: Color): String = color match
    case Color.White => "White"
    case Color.Black => "Black"
