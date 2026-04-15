package ch.tichess

import ch.tichess.model.*
import ch.tichess.view.{GuiViewAdapter, GuiViewState}
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos as FxPos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, ChoiceBox, Label, ListView, TextArea, TextField}
import scalafx.scene.layout.{BorderPane, GridPane, HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.Color as FxColor
import scalafx.scene.shape.Circle
import scalafx.scene.text.Font

object GuiMain extends JFXApp3:
  private val squareSize = 68
  private val lightSquare = "#f0d9b5"
  private val darkSquare = "#b58863"

  private final case class SquareNode(container: StackPane, pieceLabel: Label, targetMarker: Circle)

  private val adapter = new GuiViewAdapter()

  override def start(): Unit =
    var state = adapter.initialState

    val statusLabel = new Label {
      style = "-fx-font-size: 15px; -fx-font-weight: bold;"
    }

    val infoLabel = new Label {
      style = "-fx-font-size: 13px; -fx-text-fill: #334155;"
    }

    val moveList = new ListView[String] {
      prefWidth = 250
      minHeight = squareSize * 8
    }

    val fenField = new TextField {
      promptText = "fen <placement> <w|b>"
    }

    val parserChoice = new ChoiceBox[String] {
      items = ObservableBuffer.from(NotationParsers.ids)
      value = state.selectedParserId
    }

    val notationArea = new TextArea {
      promptText = "FEN oder PGN einfügen oder per Export erzeugen"
      prefRowCount = 10
      wrapText = true
    }

    var refreshUi: () => Unit = () => ()

    def updateState(next: GuiViewState): Unit =
      state = next
      refreshUi()

    val promotionLabel = new Label {
      style = "-fx-font-size: 13px; -fx-font-weight: bold; -fx-text-fill: #334155;"
      visible = false
      managed = false
      text = "Promotion:"
    }

    def promotionButton(textValue: String, role: PromotionRole): Button =
      new Button(textValue) {
        visible = false
        managed = false
        onAction = _ => updateState(GuiViewAdapter.choosePromotion(state, role))
      }

    val queenButton = promotionButton("Dame", PromotionRole.Queen)
    val rookButton = promotionButton("Turm", PromotionRole.Rook)
    val bishopButton = promotionButton("Läufer", PromotionRole.Bishop)
    val knightButton = promotionButton("Springer", PromotionRole.Knight)
    val cancelPromotionButton = new Button("Abbrechen") {
      visible = false
      managed = false
      onAction = _ => updateState(GuiViewAdapter.cancelPromotion(state))
    }

    val drawOfferButton = new Button("Remis anbieten") {
      onAction = _ => updateState(GuiViewAdapter.drawOffer(state))
    }

    val drawAcceptButton = new Button("Remis annehmen") {
      visible = false
      managed = false
      onAction = _ => updateState(GuiViewAdapter.drawAccept(state))
    }

    val resignButton = new Button("Aufgeben") {
      style = "-fx-background-color: #ef4444; -fx-text-fill: white;"
      onAction = _ => updateState(GuiViewAdapter.resign(state))
    }

    val newGameButton = new Button("Neues Spiel") {
      style = "-fx-background-color: #3b82f6; -fx-text-fill: white;"
      onAction = _ => updateState(GuiViewAdapter.newGame())
    }

    val blackCapturedLabel = new Label {
      style = "-fx-font-size: 18px;"
    }
    val whiteCapturedLabel = new Label {
      style = "-fx-font-size: 18px;"
    }

    val (boardGrid, squares) = buildBoardGrid(updateState, () => state)
    refreshUi = () =>
      render(
        state,
        promotionLabel,
        Seq(queenButton, rookButton, bishopButton, knightButton),
        cancelPromotionButton,
        drawOfferButton,
        drawAcceptButton,
        resignButton,
        newGameButton,
        statusLabel,
        infoLabel,
        moveList,
        parserChoice,
        notationArea,
        squares,
        whiteCapturedLabel,
        blackCapturedLabel
      )

    val setButton = new Button("Set") {
      onAction = _ =>
        updateState(GuiViewAdapter.setFen(state, fenField.text.value))
    }

    val exportFenButton = new Button("Export FEN") {
      onAction = _ => updateState(GuiViewAdapter.exportFen(state))
    }

    val importPgnButton = new Button("Import PGN") {
      onAction = _ => updateState(GuiViewAdapter.setPgn(state, notationArea.text.value))
    }

    val exportPgnButton = new Button("Export PGN") {
      onAction = _ => updateState(GuiViewAdapter.exportPgn(state))
    }

    fenField.onAction = _ =>
      updateState(GuiViewAdapter.setFen(state, fenField.text.value))

    parserChoice.onAction = _ =>
      Option(parserChoice.value.value).foreach { parserId =>
        updateState(GuiViewAdapter.setParser(state, parserId))
      }

    val fenRow = new HBox {
      spacing = 10
      alignment = FxPos.CenterLeft
      children = Seq(new Label("Parser:"), parserChoice, new Label("FEN:"), fenField, setButton, exportFenButton)
    }
    HBox.setHgrow(fenField, Priority.Always)

    val notationPanel = new VBox {
      spacing = 8
      children = Seq(
        new Label("Notation") {
          style = "-fx-font-size: 14px; -fx-font-weight: bold;"
        },
        notationArea,
        new HBox {
          spacing = 10
          alignment = FxPos.CenterLeft
          children = Seq(importPgnButton, exportPgnButton)
        }
      )
    }

    val movePanel = new VBox {
      spacing = 8
      children = Seq(
        new Label("Move List (B/W)") {
          style = "-fx-font-size: 14px; -fx-font-weight: bold;"
        },
        moveList
      )
    }

    val boardWithCaptures = new VBox {
      spacing = 4
      children = Seq(blackCapturedLabel, boardGrid, whiteCapturedLabel)
    }

    val centerContent = new HBox {
      spacing = 18
      alignment = FxPos.TopLeft
      children = Seq(boardWithCaptures, movePanel, notationPanel)
    }

    val statusBar = new VBox {
      spacing = 4
      children = Seq(
        new HBox {
          spacing = 8
          alignment = FxPos.CenterLeft
          children = Seq(
            promotionLabel,
            queenButton,
            rookButton,
            bishopButton,
            knightButton,
            cancelPromotionButton
          )
        },
        new HBox {
          spacing = 8
          alignment = FxPos.CenterLeft
          children = Seq(drawOfferButton, drawAcceptButton, resignButton, newGameButton)
        },
        statusLabel,
        infoLabel
      )
    }

    stage = new JFXApp3.PrimaryStage {
      title = "TiChess GUI (ScalaFX)"
      minWidth = 900
      minHeight = 700
      scene = new Scene {
        root = new BorderPane {
          padding = Insets(14)
          top = fenRow
          center = centerContent
          bottom = statusBar
        }
      }
    }

    refreshUi()

  private def buildBoardGrid(updateState: GuiViewState => Unit, currentState: () => GuiViewState): (GridPane, Map[Pos, SquareNode]) =
    val grid = new GridPane {
      hgap = 0
      vgap = 0
      alignment = FxPos.CenterLeft
    }
    val squares =
      (for
        row <- 0 until 8
        rank = 7 - row
        file <- 0 until 8
        pos = Pos(file, rank)
        node = createSquare(pos, updateState, currentState)
      yield pos -> node).toMap

    for file <- 0 until 8 do
      val fileText = s"${('a' + file).toChar}"
      grid.add(coordLabel(fileText), file + 1, 0)
      grid.add(coordLabel(fileText), file + 1, 9)

    for row <- 0 until 8 do
      val rank = 7 - row
      val rankText = s"${rank + 1}"
      grid.add(coordLabel(rankText), 0, row + 1)
      grid.add(coordLabel(rankText), 9, row + 1)

      for file <- 0 until 8 do
        val pos = Pos(file, rank)
        val node = squares(pos)
        grid.add(node.container, file + 1, row + 1)

    (grid, squares)

  private def createSquare(pos: Pos, updateState: GuiViewState => Unit, currentState: () => GuiViewState): SquareNode =
    val pieceLabel = new Label {
      font = Font.font(46)
      mouseTransparent = true
    }

    val targetMarker = new Circle {
      radius = 10
      fill = FxColor.web("#16a34a", 0.45)
      visible = false
      mouseTransparent = true
    }

    val square = new StackPane {
      minWidth = squareSize
      minHeight = squareSize
      prefWidth = squareSize
      prefHeight = squareSize
      maxWidth = squareSize
      maxHeight = squareSize
      alignment = FxPos.Center
      children = Seq(targetMarker, pieceLabel)
      onMouseClicked = _ =>
        updateState(GuiViewAdapter.handleSquareClick(currentState(), pos))
    }

    SquareNode(square, pieceLabel, targetMarker)

  private def coordLabel(text: String): Label =
    new Label(text) {
      minWidth = 26
      minHeight = 26
      alignment = FxPos.Center
      style = "-fx-font-size: 13px; -fx-text-fill: #475569;"
    }

  private def render(
      state: GuiViewState,
      promotionLabel: Label,
      promotionButtons: Seq[Button],
      cancelPromotionButton: Button,
      drawOfferButton: Button,
      drawAcceptButton: Button,
      resignButton: Button,
      newGameButton: Button,
      statusLabel: Label,
      infoLabel: Label,
      moveList: ListView[String],
      parserChoice: ChoiceBox[String],
      notationArea: TextArea,
      squares: Map[Pos, SquareNode],
      whiteCapturedLabel: Label,
      blackCapturedLabel: Label
  ): Unit =
    val board = state.game.board
    val selected = state.selectedPos
    val legalTargets = state.legalTargetSquares
    val gameOver = state.isGameOver
    val promotionPending = state.pendingPromotion.nonEmpty
    val drawPending = state.drawOfferedBy.nonEmpty && !state.drawAgreed

    promotionLabel.visible = promotionPending
    promotionLabel.managed = promotionPending
    promotionButtons.foreach { button =>
      button.visible = promotionPending
      button.managed = promotionPending
    }
    cancelPromotionButton.visible = promotionPending
    cancelPromotionButton.managed = promotionPending

    drawOfferButton.disable = gameOver || promotionPending || drawPending
    drawAcceptButton.visible = drawPending
    drawAcceptButton.managed = drawPending

    resignButton.disable = gameOver

    squares.foreach { (pos, square) =>
      val pieceOpt = board.pieceAt(pos)
      val selectedHere = selected.contains(pos)
      val legalHere = legalTargets.contains(pos)

      val baseColor =
        if (pos.file + pos.rank) % 2 == 0 then darkSquare else lightSquare

      val borderColor =
        if selectedHere then "#f59e0b"
        else if legalHere then "#16a34a"
        else "#00000033"

      val borderWidth =
        if selectedHere || legalHere then 3 else 1

      square.container.style =
        s"-fx-background-color: $baseColor; -fx-border-color: $borderColor; -fx-border-width: $borderWidth;"
      square.container.disable = gameOver || promotionPending
      square.targetMarker.visible = legalHere && pieceOpt.isEmpty

      pieceOpt match
        case None =>
          square.pieceLabel.text = ""
        case Some(piece) =>
          square.pieceLabel.text = pieceSymbol(piece)
          square.pieceLabel.textFill =
            piece.color match
              case Color.White => FxColor.web("#ffffff")
              case Color.Black => FxColor.web("#000000")
          square.pieceLabel.style =
            piece.color match
              case Color.White => "-fx-effect: dropshadow(gaussian, rgba(0,0,0,0.65), 2, 0.3, 0, 0);"
              case Color.Black => ""
    }

    statusLabel.text = state.statusText
    infoLabel.text = state.infoMessage.getOrElse("")
    moveList.items = ObservableBuffer.from(state.moveEntries)
    parserChoice.value = state.selectedParserId
    if notationArea.text.value != state.notationText then notationArea.text = state.notationText

    val adv = state.materialAdvantage
    whiteCapturedLabel.text = capturedDisplay(state.capturedByWhite, adv, adv > 0)
    blackCapturedLabel.text = capturedDisplay(state.capturedByBlack, adv, adv < 0)

  private def capturedDisplay(pieces: List[PieceType], advantage: Int, showAdv: Boolean): String =
    val symbols = pieces.map(capturedPieceChar).mkString
    val advStr = if showAdv then s"  +${Math.abs(advantage)}" else ""
    if symbols.isEmpty && !showAdv then "" else s"$symbols$advStr"

  private def capturedPieceChar(kind: PieceType): String = kind match
    case PieceType.Pawn   => "♟"
    case PieceType.Knight => "♞"
    case PieceType.Bishop => "♝"
    case PieceType.Rook   => "♜"
    case PieceType.Queen  => "♛"
    case PieceType.King   => "♚"

  private def pieceSymbol(piece: Piece): String =
    piece.kind match
      case PieceType.King   => "♚"
      case PieceType.Queen  => "♛"
      case PieceType.Rook   => "♜"
      case PieceType.Bishop => "♝"
      case PieceType.Knight => "♞"
      case PieceType.Pawn   => "♟"
