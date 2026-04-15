package ch.tichess.view

import ch.tichess.model.*

final case class PendingPromotion(from: Pos, to: Pos, color: Color)

final case class GuiViewState(
    game: Game,
    startGame: Game = Game.initial,
    moveHistory: Vector[Move] = Vector.empty,
    selectedPos: Option[Pos] = None,
    legalTargetSquares: Set[Pos] = Set.empty,
    infoMessage: Option[String] = None,
    moveEntries: Vector[String] = Vector.empty,
    pendingPromotion: Option[PendingPromotion] = None,
    selectedParserId: String = NotationParsers.default.id,
    notationText: String = "",
    drawOfferedBy: Option[Color] = None,
    drawAgreed: Boolean = false
):
  def isGameOver: Boolean = game.isCheckmate || game.isDraw || drawAgreed

  def parserChoice: ParserChoice =
    NotationParsers.resolve(selectedParserId).getOrElse(NotationParsers.default)

  def statusText: String =
    if drawAgreed then "Remis - Einigung"
    else if game.isCheckmate then s"Schachmatt - ${GuiViewAdapter.colorLabel(game.sideToMove.other)} gewinnt"
    else if game.isDraw then
      if game.halfMoveClock >= 100 then "Remis - 50-Züge-Regel"
      else "Patt - Remis"
    else
      val turn = s"${GuiViewAdapter.colorLabel(game.sideToMove)} to move"
      val drawNote = drawOfferedBy.map(c => s" | ${GuiViewAdapter.colorLabel(c)} bietet Remis an").getOrElse("")
      if game.isInCheck then s"$turn | Schach$drawNote" else s"$turn$drawNote"

  private def pieceValue(kind: PieceType): Int = kind match
    case PieceType.Pawn   => 1
    case PieceType.Knight => 3
    case PieceType.Bishop => 3
    case PieceType.Rook   => 5
    case PieceType.Queen  => 9
    case PieceType.King   => 0

  private def computeCaptured(opponentColor: Color): List[PieceType] =
    val startPieces = startGame.board.allPieces.values.filter(_.color == opponentColor).toList
    val currentPieces = game.board.allPieces.values.filter(_.color == opponentColor).toList
    val startCounts = startPieces.groupBy(_.kind).view.mapValues(_.size).toMap
    val currentCounts = currentPieces.groupBy(_.kind).view.mapValues(_.size).toMap
    (startCounts.keySet ++ currentCounts.keySet).toList.flatMap { kind =>
      val diff = startCounts.getOrElse(kind, 0) - currentCounts.getOrElse(kind, 0)
      if diff > 0 then List.fill(diff)(kind) else Nil
    }.sortBy(pieceValue)

  /** Piece types White captured from Black, sorted by value ascending */
  def capturedByWhite: List[PieceType] = computeCaptured(Color.Black)

  /** Piece types Black captured from White, sorted by value ascending */
  def capturedByBlack: List[PieceType] = computeCaptured(Color.White)

  /** Positive = White leads, negative = Black leads */
  def materialAdvantage: Int =
    capturedByWhite.map(pieceValue).sum - capturedByBlack.map(pieceValue).sum

object GuiViewState:
  val initial: GuiViewState = GuiViewState(Game.initial, startGame = Game.initial, moveHistory = Vector.empty)

final class GuiViewAdapter(initialGame: Game = Game.initial):
  def initialState: GuiViewState = GuiViewState(initialGame, startGame = initialGame, moveHistory = Vector.empty)

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

  def setParser(state: GuiViewState, parserId: String): GuiViewState =
    NotationParsers.resolve(parserId) match
      case Left(err) => state.copy(infoMessage = Some(err))
      case Right(choice) =>
        state.copy(selectedParserId = choice.id, infoMessage = Some(s"Parser gesetzt: ${choice.id}."))

  def setFen(state: GuiViewState, fen: String): GuiViewState =
    state.parserChoice.fenParser.parse(fen.trim) match
      case Left(err) => state.copy(infoMessage = Some(err))
      case Right(next) =>
        clearSelection(
          state.copy(
            game = next,
            startGame = next,
            moveHistory = Vector.empty,
            moveEntries = Vector.empty,
            infoMessage = Some(s"Position gesetzt mit ${state.parserChoice.id}."),
            pendingPromotion = None,
            notationText = fen.trim
          )
        )

  def exportFen(state: GuiViewState): GuiViewState =
    state.copy(notationText = Fen.encode(state.game), infoMessage = Some("FEN exportiert."))

  def setPgn(state: GuiViewState, pgn: String): GuiViewState =
    Pgn.parse(pgn.trim, state.parserChoice) match
      case Left(err) => state.copy(infoMessage = Some(err))
      case Right(imported) =>
        val base = clearSelection(
          state.copy(
            game = imported.game,
            startGame = imported.startGame,
            moveHistory = imported.moves,
            moveEntries = buildMoveEntries(imported.startGame, imported.moves),
            pendingPromotion = None,
            notationText = pgn.trim,
            drawOfferedBy = None
          )
        )
        if imported.game.isCheckmate then
          val winner = colorLabel(imported.game.sideToMove.other)
          base.copy(infoMessage = Some(s"Schachmatt - $winner gewinnt"))
        else if imported.game.isDraw then
          base.copy(infoMessage = Some("Patt - Remis"))
        else imported.result match
          case "1/2-1/2" =>
            base.copy(drawAgreed = true, infoMessage = Some("Remis (laut PGN)."))
          case "1-0" =>
            base.copy(drawAgreed = true, infoMessage = Some("White wins (laut PGN)."))
          case "0-1" =>
            base.copy(drawAgreed = true, infoMessage = Some("Black wins (laut PGN)."))
          case _ =>
            base.copy(infoMessage = Some(s"PGN importiert mit ${state.parserChoice.id}."))

  def exportPgn(state: GuiViewState): GuiViewState =
    val result = if state.drawAgreed then Some("1/2-1/2") else None
    state.copy(notationText = Pgn.encode(state.startGame, state.moveHistory, result), infoMessage = Some("PGN exportiert."))

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

  def drawOffer(state: GuiViewState): GuiViewState =
    if state.isGameOver then state
    else
      val offerer = state.game.sideToMove
      state.copy(
        drawOfferedBy = Some(offerer),
        infoMessage = Some(s"${colorLabel(offerer)} bietet Remis an. Zum Annehmen 'Remis annehmen' klicken.")
      )

  def drawAccept(state: GuiViewState): GuiViewState =
    state.drawOfferedBy match
      case Some(_) =>
        state.copy(
          drawAgreed = true,
          drawOfferedBy = None,
          infoMessage = Some("Spiel durch Remis-Übereinkunft beendet.")
        )
      case None =>
        state.copy(infoMessage = Some("Kein Remis-Angebot vorhanden."))

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
    state.game.applyMove(move) match
      case Left(err) => state.copy(infoMessage = Some(err))
      case Right(next) =>
        // A move clears the pending draw offer from the opponent
        val updated = clearSelection(
          state.copy(
            game = next,
            moveHistory = state.moveHistory :+ move,
            moveEntries = updateMoveEntries(state.moveEntries, state.game, move, mover, next),
            infoMessage = pendingInfo.orElse(state.infoMessage),
            drawOfferedBy = None
          )
        )
        if next.isCheckmate then updated.copy(infoMessage = Some(s"Schachmatt - ${colorLabel(mover)} gewinnt"))
        else if next.isDraw then
          val drawMsg = if next.halfMoveClock >= 100 then "Remis - 50-Züge-Regel" else "Patt - Remis"
          updated.copy(infoMessage = Some(drawMsg))
        else if next.isInCheck then updated.copy(infoMessage = Some("Schach"))
        else updated.copy(infoMessage = None)

  private def legalMovesFrom(game: Game, from: Pos): Set[Pos] =
    game.legalMoves.collect { case Move(`from`, to, _) => to }.toSet

  private def updateMoveEntries(
      entries: Vector[String],
      gameBefore: Game,
      move: Move,
      mover: Color,
      gameAfter: Game
  ): Vector[String] =
    val san = toSAN(gameBefore, move, gameAfter)
    mover match
      case Color.White =>
        val moveNum = entries.size + 1
        entries :+ f"$moveNum%-4d $san"
      case Color.Black =>
        if entries.nonEmpty then
          val padded = f"${entries.last}%-18s"
          entries.updated(entries.size - 1, s"$padded $san")
        else
          entries :+ s"...  $san"

  private def toSAN(game: Game, move: Move, gameAfter: Game): String =
    val piece = game.board.pieceAt(move.from).get
    val isCapture = game.board.pieceAt(move.to).isDefined ||
      (piece.kind == PieceType.Pawn && game.enPassantTarget.contains(move.to))

    val base =
      if piece.kind == PieceType.King && Math.abs(move.to.file - move.from.file) == 2 then
        if move.to.file > move.from.file then "O-O" else "O-O-O"
      else
        val pieceChar = piece.kind match
          case PieceType.King   => "K"
          case PieceType.Queen  => "D"
          case PieceType.Rook   => "T"
          case PieceType.Bishop => "L"
          case PieceType.Knight => "S"
          case PieceType.Pawn   => ""

        val disambig =
          if piece.kind != PieceType.Pawn then disambiguate(game, move, piece)
          else if isCapture then s"${('a' + move.from.file).toChar}"
          else ""

        val captureStr = if isCapture then "x" else ""
        val dest = toAlg(move.to)
        val promoStr = move.promotion.map {
          case PromotionRole.Queen  => "=D"
          case PromotionRole.Rook   => "=T"
          case PromotionRole.Bishop => "=L"
          case PromotionRole.Knight => "=S"
        }.getOrElse("")
        s"$pieceChar$disambig$captureStr$dest$promoStr"

    val suffix =
      if gameAfter.isCheckmate then "#"
      else if gameAfter.isInCheck then "+"
      else ""
    s"$base$suffix"

  private def disambiguate(game: Game, move: Move, piece: Piece): String =
    val others = game.legalMoves.filter { m =>
      m.to == move.to &&
      m.from != move.from &&
      game.board.pieceAt(m.from).exists(p => p.kind == piece.kind && p.color == piece.color)
    }
    if others.isEmpty then ""
    else
      val sameFile = others.exists(_.from.file == move.from.file)
      val sameRank = others.exists(_.from.rank == move.from.rank)
      if !sameFile then s"${('a' + move.from.file).toChar}"
      else if !sameRank then s"${move.from.rank + 1}"
      else s"${('a' + move.from.file).toChar}${move.from.rank + 1}"

  private[view] def buildMoveEntries(startGame: Game, moves: Vector[Move]): Vector[String] =
    val (_, entries) =
      moves.foldLeft((startGame, Vector.empty[String])) { case ((game, log), move) =>
        val mover = game.sideToMove
        game.applyMove(move) match
          case Right(next) =>
            val newLog = updateMoveEntries(log, game, move, mover, next)
            (next, newLog)
          case Left(_) =>
            (game, log)
      }
    entries

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
