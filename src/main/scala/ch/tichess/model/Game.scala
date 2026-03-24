package ch.tichess.model

final case class Game(board: Board, sideToMove: Color):
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
