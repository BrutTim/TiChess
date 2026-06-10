package ch.tichess.bot

import ch.tichess.model.{Color, Game, PieceType, Pos}

import scala.util.Random

object ZobristHash:
  private val random = Random(0x5EEDBEEFL)

  private val pieceSquareKeys: Array[Array[Array[Long]]] =
    Array.fill(2, PieceType.values.length, 64)(random.nextLong())

  private val sideToMoveKey: Long = random.nextLong()
  private val castlingKeys: Array[Long] = Array.fill(4)(random.nextLong())
  private val enPassantFileKeys: Array[Long] = Array.fill(8)(random.nextLong())
  private val halfMoveClockKeys: Array[Long] = Array.fill(101)(random.nextLong())

  def hash(game: Game): Long =
    var key = 0L

    game.board.allPieces.foreach { (pos, piece) =>
      key ^= pieceSquareKeys(colorIndex(piece.color))(piece.kind.ordinal)(squareIndex(pos))
    }

    if game.sideToMove == Color.Black then key ^= sideToMoveKey

    if game.castlingRights.whiteKingside then key ^= castlingKeys(0)
    if game.castlingRights.whiteQueenside then key ^= castlingKeys(1)
    if game.castlingRights.blackKingside then key ^= castlingKeys(2)
    if game.castlingRights.blackQueenside then key ^= castlingKeys(3)

    game.enPassantTarget.foreach { pos =>
      key ^= enPassantFileKeys(pos.file)
    }

    key ^ halfMoveClockKeys(Math.min(game.halfMoveClock, 100))

  private def colorIndex(color: Color): Int =
    color match
      case Color.White => 0
      case Color.Black => 1

  private def squareIndex(pos: Pos): Int =
    pos.rank * 8 + pos.file