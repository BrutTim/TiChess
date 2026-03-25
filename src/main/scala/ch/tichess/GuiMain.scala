package ch.tichess

import ch.tichess.model.*
import ch.tichess.view.GuiViewAdapter
import scalafx.application.JFXApp3
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos as FxPos}
import scalafx.scene.Scene
import scalafx.scene.control.{Button, Label, ListView, TextField}
import scalafx.scene.layout.{BorderPane, GridPane, HBox, Priority, StackPane, VBox}
import scalafx.scene.paint.Color as FxColor
import scalafx.scene.shape.Circle
import scalafx.scene.text.{Font, FontWeight}

import scala.collection.mutable

object GuiMain extends JFXApp3:
  private val squareSize = 68
  private val lightSquare = "#f0d9b5"
  private val darkSquare = "#b58863"

  private final case class SquareNode(container: StackPane, pieceLabel: Label, targetMarker: Circle)

  private val adapter = new GuiViewAdapter()
  private val squares = mutable.Map.empty[Pos, SquareNode]
  private var statusLabel: Label = null
  private var infoLabel: Label = null
  private var moveList: ListView[String] = null
  private var fenField: TextField = null

  override def start(): Unit =
    statusLabel = new Label {
      style = "-fx-font-size: 15px; -fx-font-weight: bold;"
    }

    infoLabel = new Label {
      style = "-fx-font-size: 13px; -fx-text-fill: #334155;"
    }

    moveList = new ListView[String] {
      prefWidth = 250
      minHeight = squareSize * 8
    }

    fenField = new TextField {
      promptText = "fen <placement> <w|b>"
    }

    val boardGrid = buildBoardGrid()

    val setButton = new Button("Set") {
      onAction = _ =>
        adapter.setFen(fenField.text.value)
        refreshUi()
    }

    fenField.onAction = _ =>
      adapter.setFen(fenField.text.value)
      refreshUi()

    val fenRow = new HBox {
      spacing = 10
      alignment = FxPos.CenterLeft
      children = Seq(new Label("FEN:"), fenField, setButton)
    }
    HBox.setHgrow(fenField, Priority.Always)

    val movePanel = new VBox {
      spacing = 8
      children = Seq(
        new Label("Move List (B/W)") {
          style = "-fx-font-size: 14px; -fx-font-weight: bold;"
        },
        moveList
      )
    }

    val centerContent = new HBox {
      spacing = 18
      alignment = FxPos.TopLeft
      children = Seq(boardGrid, movePanel)
    }

    val statusBar = new VBox {
      spacing = 4
      children = Seq(statusLabel, infoLabel)
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

  private def buildBoardGrid(): GridPane =
    val grid = new GridPane {
      hgap = 0
      vgap = 0
      alignment = FxPos.CenterLeft
    }

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
        val node = createSquare(pos)
        squares += pos -> node
        grid.add(node.container, file + 1, row + 1)

    grid

  private def createSquare(pos: Pos): SquareNode =
    val pieceLabel = new Label {
      font = Font.font("Menlo", FontWeight.Bold, 30)
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
        adapter.handleSquareClick(pos)
        refreshUi()
    }

    SquareNode(square, pieceLabel, targetMarker)

  private def coordLabel(text: String): Label =
    new Label(text) {
      minWidth = 26
      minHeight = 26
      alignment = FxPos.Center
      style = "-fx-font-size: 13px; -fx-text-fill: #475569;"
    }

  private def refreshUi(): Unit =
    val board = adapter.game.board
    val selected = adapter.selectedPos
    val legalTargets = adapter.legalTargetSquares
    val gameOver = adapter.isGameOver

    squares.foreach { (pos, square) =>
      val pieceOpt = board.pieceAt(pos)
      val selectedHere = selected.contains(pos)
      val legalHere = legalTargets.contains(pos)

      val baseColor =
        if (pos.file + pos.rank) % 2 == 0 then lightSquare else darkSquare

      val borderColor =
        if selectedHere then "#f59e0b"
        else if legalHere then "#16a34a"
        else "#00000033"

      val borderWidth =
        if selectedHere || legalHere then 3 else 1

      square.container.style =
        s"-fx-background-color: $baseColor; -fx-border-color: $borderColor; -fx-border-width: $borderWidth;"
      square.container.disable = gameOver
      square.targetMarker.visible = legalHere && pieceOpt.isEmpty

      pieceOpt match
        case None =>
          square.pieceLabel.text = ""
        case Some(piece) =>
          square.pieceLabel.text = pieceSymbol(piece)
          square.pieceLabel.textFill = piece.color match
            case Color.White => FxColor.web("#1d4ed8")
            case Color.Black => FxColor.web("#b91c1c")
    }

    statusLabel.text = adapter.statusText
    infoLabel.text = adapter.infoMessage.getOrElse("")
    moveList.items = ObservableBuffer.from(adapter.moveEntries)

  private def pieceSymbol(piece: Piece): String =
    val base = piece.kind match
      case PieceType.King   => "k"
      case PieceType.Queen  => "q"
      case PieceType.Rook   => "r"
      case PieceType.Bishop => "b"
      case PieceType.Knight => "n"
      case PieceType.Pawn   => "p"

    piece.color match
      case Color.White => base.toUpperCase
      case Color.Black => base
