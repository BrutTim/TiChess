package ch.tichess.model

final case class Game(board: Board, sideToMove: Color):
  def applyMove(move: Move): Either[String, Game] =
    for
      _ <- Rules.validateMove(board, sideToMove, move)
      nextBoard <- board.movePiece(move)
    yield Game(nextBoard, sideToMove.other)

object Game:
  def initial: Game = Game(Board.initial, Color.White)
