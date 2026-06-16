package ch.tichess.model

object Rules:
  private def promotionRank(color: Color, rank: Int): Boolean =
    (color == Color.White && rank == 7) || (color == Color.Black && rank == 0)

  private def validatePromotion(piece: Piece, move: Move): Either[String, Unit] =
    val reachesPromotionRank = piece.kind == PieceType.Pawn && promotionRank(piece.color, move.to.rank)

    move.promotion match
      case Some(_) if !reachesPromotionRank =>
        Left("Promotion is only allowed for pawns reaching the back rank.")
      case None if reachesPromotionRank =>
        Right(())
      case _ =>
        Right(())

  private[model] def sign(x: Int): Int = if x == 0 then 0 else if x > 0 then 1 else -1

  private[model] def squaresBetweenExclusive(from: Pos, to: Pos): List[Pos] =
    val df = to.file - from.file
    val dr = to.rank - from.rank
    val stepF = sign(df)
    val stepR = sign(dr)
    val isLine =
      (df == 0 && dr != 0) ||
        (dr == 0 && df != 0) ||
        (Math.abs(df) == Math.abs(dr) && df != 0)

    if !isLine then Nil
    else
      val steps = (Math.max(Math.abs(df), Math.abs(dr)) - 1).max(0)
      (1 to steps).toList.map(i => Pos(from.file + stepF * i, from.rank + stepR * i))

  private[model] def clearPath(board: Board, from: Pos, to: Pos): Boolean =
    BitboardAttacks.clearPath(board.bitboards, from, to)

  private[model] def findKing(board: Board, color: Color): Option[Pos] =
    board.bitboards.kingSquare(color)

  def isInCheck(board: Board, color: Color): Boolean =
    BitboardAttacks.isInCheck(board.bitboards, color)

  def isDraw(game: Game): Boolean =
    game.halfMoveClock >= 100 || (!game.isInCheck && game.legalMoves.isEmpty)

  def validateMove(game: Game, move: Move): Either[String, Unit] =
    val board = game.board
    val sideToMove = game.sideToMove
    if move.from == move.to then Left("Source and destination must differ.")
    else
      board.pieceAt(move.from) match
        case None => Left("No piece at source position.")
        case Some(piece) if piece.color != sideToMove => Left("Not your piece.")
        case Some(piece) =>
          for
            _ <-
              board.pieceAt(move.to) match
                case Some(target) if target.color == sideToMove => Left("Cannot capture your own piece.")
                case _                                          => Right(())
            _ <- validatePromotion(piece, move)
            _ <-
              val df = move.to.file - move.from.file
              val dr = move.to.rank - move.from.rank
              val absDf = Math.abs(df)
              val absDr = Math.abs(dr)

              piece.kind match
                case PieceType.King =>
                  if absDf <= 1 && absDr <= 1 then Right(())
                  else if absDf == 2 && absDr == 0 && move.from.file == 4 then
                    val rank = if sideToMove == Color.White then 0 else 7
                    if move.from.rank != rank then Left("Illegal castling rank.")
                    else if game.isInCheck then Left("Cannot castle out of check.")
                    else
                      val isKingside = df > 0
                      val rightsOk = sideToMove match
                        case Color.White => if isKingside then game.castlingRights.whiteKingside else game.castlingRights.whiteQueenside
                        case Color.Black => if isKingside then game.castlingRights.blackKingside else game.castlingRights.blackQueenside
                      
                      if !rightsOk then Left("Castling rights lost.")
                      else if !clearPath(board, move.from, Pos(if isKingside then 7 else 0, rank)) then Left("Castling path not clear.")
                      else if isInCheck(board.movePiece(Move(move.from, Pos(move.from.file + sign(df), rank))).toOption.get, sideToMove) then
                        Left("Cannot castle through check.")
                      else Right(())
                  else Left("Illegal king move.")

                case PieceType.Queen =>
                  val okLine = (df == 0 && dr != 0) || (dr == 0 && df != 0) || (absDf == absDr && df != 0)
                  if okLine && clearPath(board, move.from, move.to) then Right(())
                  else Left("Illegal queen move.")

                case PieceType.Rook =>
                  val okLine = (df == 0 && dr != 0) || (dr == 0 && df != 0)
                  if okLine && clearPath(board, move.from, move.to) then Right(())
                  else Left("Illegal rook move.")

                case PieceType.Bishop =>
                  val okLine = absDf == absDr && df != 0
                  if okLine && clearPath(board, move.from, move.to) then Right(())
                  else Left("Illegal bishop move.")

                case PieceType.Knight =>
                  val ok = (absDf == 2 && absDr == 1) || (absDf == 1 && absDr == 2)
                  if ok then Right(()) else Left("Illegal knight move.")

                case PieceType.Pawn =>
                  val dir = if sideToMove == Color.White then 1 else -1
                  val startRank = if sideToMove == Color.White then 1 else 6
                  val oneForward = df == 0 && dr == dir && board.isEmpty(move.to)
                  val twoForward =
                    df == 0 && dr == 2 * dir && move.from.rank == startRank &&
                      board.isEmpty(move.to) &&
                      board.isEmpty(Pos(move.from.file, move.from.rank + dir))
                  val isCapture = board.pieceAt(move.to).exists(_.color != sideToMove)
                  val isEnPassant = game.enPassantTarget.contains(move.to)
                  val captureDiag = absDf == 1 && dr == dir && (isCapture || isEnPassant)

                  if oneForward || twoForward || captureDiag then Right(())
                  else Left("Illegal pawn move.")
          yield ()
