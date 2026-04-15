package ch.tichess.view

import ch.tichess.model.*

object ConsoleView:
  def render(game: Game, message: Option[String] = None, startGame: Game = Game.initial): String =
    val header = message.map(m => s"$m\n").getOrElse("")
    val turnLine = s"${colorLabel(game.sideToMove)} to move\n"
    val captured = capturedDisplay(game, startGame)
    header + turnLine + captured + renderBoard(game.board)

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

  private def capturedDisplay(game: Game, startGame: Game): String =
    val whiteCap = computeCaptured(startGame.board, game.board, Color.Black)
    val blackCap = computeCaptured(startGame.board, game.board, Color.White)
    val adv = whiteCap.map(pieceValue).sum - blackCap.map(pieceValue).sum
    val wLine = formatCapLine("White", whiteCap, adv, adv > 0)
    val bLine = formatCapLine("Black", blackCap, adv, adv < 0)
    if whiteCap.isEmpty && blackCap.isEmpty then ""
    else s"$bLine\n$wLine\n"

  private def formatCapLine(side: String, pieces: List[PieceType], adv: Int, showAdv: Boolean): String =
    val chars = pieces.map(capturedChar).mkString
    val advStr = if showAdv then s" +${Math.abs(adv)}" else ""
    if chars.isEmpty && !showAdv then s"$side captured: -"
    else s"$side captured: $chars$advStr"

  private def computeCaptured(startBoard: Board, currentBoard: Board, opponentColor: Color): List[PieceType] =
    val startPieces = startBoard.allPieces.values.filter(_.color == opponentColor).toList
    val currentPieces = currentBoard.allPieces.values.filter(_.color == opponentColor).toList
    val startCounts = startPieces.groupBy(_.kind).view.mapValues(_.size).toMap
    val currentCounts = currentPieces.groupBy(_.kind).view.mapValues(_.size).toMap
    (startCounts.keySet ++ currentCounts.keySet).toList.flatMap { kind =>
      val diff = startCounts.getOrElse(kind, 0) - currentCounts.getOrElse(kind, 0)
      if diff > 0 then List.fill(diff)(kind) else Nil
    }.sortBy(pieceValue)

  private def pieceValue(kind: PieceType): Int = kind match
    case PieceType.Pawn   => 1
    case PieceType.Knight => 3
    case PieceType.Bishop => 3
    case PieceType.Rook   => 5
    case PieceType.Queen  => 9
    case PieceType.King   => 0

  private def capturedChar(kind: PieceType): Char = kind match
    case PieceType.Pawn   => 'p'
    case PieceType.Knight => 'n'
    case PieceType.Bishop => 'b'
    case PieceType.Rook   => 'r'
    case PieceType.Queen  => 'q'
    case PieceType.King   => 'k'

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
