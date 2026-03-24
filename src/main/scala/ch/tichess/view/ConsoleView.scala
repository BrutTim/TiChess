package ch.tichess.view

import ch.tichess.model.*

object ConsoleView:
  def render(game: Game, message: Option[String] = None): String =
    val header = message.map(m => s"$m\n").getOrElse("")
    val turnLine = s"${colorLabel(game.sideToMove)} to move\n"
    header + turnLine + renderBoard(game.board)

  def renderBoard(board: Board): String =
    val ranks = (7 to 0 by -1).toList
    val files = (0 until 8).toList

    val lines = ranks.map { r =>
      val row = files
        .map(f => board.pieceAt(Pos(f, r)).map(pieceChar).getOrElse('.'))
        .mkString(" ")
      s"${r + 1} $row"
    }

    val footer = "  a b c d e f g h"
    (lines :+ footer).mkString("\n")

  private def colorLabel(c: Color): String = c match
    case Color.White => "White"
    case Color.Black => "Black"

  private def pieceChar(p: Piece): Char =
    val base = p.kind match
      case PieceType.King   => 'k'
      case PieceType.Queen  => 'q'
      case PieceType.Rook   => 'r'
      case PieceType.Bishop => 'b'
      case PieceType.Knight => 'n'
      case PieceType.Pawn   => 'p'
    p.color match
      case Color.White => base.toUpper
      case Color.Black => base

