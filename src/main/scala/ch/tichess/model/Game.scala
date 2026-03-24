package ch.tichess.model

final case class Game(board: Board, sideToMove: Color):
  def isInCheck: Boolean = Rules.isInCheck(board, sideToMove)

  def legalMoves: List[Move] =
    val ownPositions =
      board.allPieces.collect { case (pos, piece) if piece.color == sideToMove => pos }.toList
    val allTargets =
      (0 until 8).flatMap(file => (0 until 8).map(rank => Pos(file, rank))).toList

    ownPositions.flatMap { from =>
      allTargets.flatMap { to =>
        val move = Move(from, to)
        Rules.validateMove(board, sideToMove, move).toOption.flatMap { _ =>
          board.movePiece(move).toOption.flatMap { nextBoard =>
            if Rules.isInCheck(nextBoard, sideToMove) then None else Some(move)
          }
        }
      }
    }

  def isCheckmate: Boolean = isInCheck && legalMoves.isEmpty

  def applyMove(move: Move): Either[String, Game] =
    for
      _ <- Rules.validateMove(board, sideToMove, move)
      nextBoard <- board.movePiece(move)
      _ <-
        if Rules.isInCheck(nextBoard, sideToMove) then Left("Illegal move: king would remain in check.")
        else Right(())
    yield Game(nextBoard, sideToMove.other)

object Game:
  def initial: Game = Game(Board.initial, Color.White)
