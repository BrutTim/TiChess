package ch.tichess.model

object Fen:

  private def pieceToChar(p: Piece): Char =
    val base = p.kind match
      case PieceType.King => 'K'
      case PieceType.Queen => 'Q'
      case PieceType.Rook => 'R'
      case PieceType.Bishop => 'B'
      case PieceType.Knight => 'N'
      case PieceType.Pawn => 'P'
    p.color match
      case Color.White => base
      case Color.Black => base.toLower

  private def charToPiece(c: Char): Either[String, Piece] =
    val color =
      c match
        case ch if ch.isUpper => Color.White
        case ch if ch.isLower => Color.Black
        case _ => return Left("Invalid piece character.")

    val kind =
      c.toLower match
        case 'k' => PieceType.King
        case 'q' => PieceType.Queen
        case 'r' => PieceType.Rook
        case 'b' => PieceType.Bishop
        case 'n' => PieceType.Knight
        case 'p' => PieceType.Pawn
        case _ => return Left("Invalid piece character.")

    Right(Piece(color, kind))

  private def parsePlacement(placement: String): Either[String, Board] =
    val rows = placement.split("/")
    if rows.length != 8 then Left("FEN placement must have 8 ranks separated by '/'.")
    else
      def parseRank(rankStr: String, rank: Int, acc0: Map[Pos, Piece]): Either[String, Map[Pos, Piece]] =
        val init: Either[String, (Int, Map[Pos, Piece])] = Right((0, acc0))

        val endState: Either[String, (Int, Map[Pos, Piece])] =
          rankStr.foldLeft(init) { (stateE, ch) =>
            stateE.flatMap { (file, m) =>
              if ch.isDigit then
                val empty = ch.asDigit
                if empty < 1 || empty > 8 then Left("FEN digit must be 1..8.")
                else if file + empty > 8 then Left("FEN rank has too many squares.")
                else Right((file + empty, m))
              else
                if file > 7 then Left("FEN rank has too many squares.")
                else
                  for
                    piece <- charToPiece(ch)
                    pos = Pos(file, rank)
                  yield (file + 1, m + (pos -> piece))
            }
          }

        endState.flatMap { (file, m) =>
          if file != 8 then Left("FEN rank does not cover exactly 8 squares.")
          else Right(m)
        }

      val parsedOrError: Either[String, Map[Pos, Piece]] =
        rows.zipWithIndex.foldLeft(Right(Map.empty): Either[String, Map[Pos, Piece]]) {
          case (accE, (rankStr, rowIdx)) =>
            accE.flatMap { acc =>
              val rank = 7 - rowIdx // FEN rank 8 -> Pos.rank 7, ..., rank 1 -> Pos.rank 0
              parseRank(rankStr, rank, acc)
            }
        }

      parsedOrError.map(Board.apply)

  private def encodePlacement(board: Board): String =
    def rowString(rank: Int): String =
      val (empties, parts) =
        (0 until 8).foldLeft((0, List.empty[String])) {
          case ((emptyCount, accParts), file) =>
            board.pieceAt(Pos(file, rank)) match
              case None => (emptyCount + 1, accParts)
              case Some(piece) =>
                val filledParts =
                  if emptyCount > 0 then accParts :+ emptyCount.toString else accParts
                (0, filledParts :+ pieceToChar(piece).toString)
        }
      if empties > 0 then (parts :+ empties.toString).mkString else parts.mkString

    (7 to 0 by -1).map(rowString).mkString("/")

  def parse(fen: String): Either[String, Game] =
    val fields = fen.trim.split("\\s+").toList
    if fields.length < 2 then Left("FEN must contain at least placement and side-to-move.")
    else
      val placement = fields.head
      val side = fields(1).toLowerCase
      val sideToMove =
        side match
          case "w" => Color.White
          case "b" => Color.Black
          case _ => return Left("FEN side-to-move must be 'w' or 'b'.")

      for
        board <- parsePlacement(placement)
        // ensure check-detection remains consistent
        _ <- validateKings(board)
      yield Game(board, sideToMove)

  private def validateKings(board: Board): Either[String, Unit] =
    val whiteKings = board.allPieces.values.count(p => p.kind == PieceType.King && p.color == Color.White)
    val blackKings = board.allPieces.values.count(p => p.kind == PieceType.King && p.color == Color.Black)
    if whiteKings != 1 then Left("FEN must contain exactly one white king.")
    else if blackKings != 1 then Left("FEN must contain exactly one black king.")
    else Right(())

  def encode(game: Game): String =
    val placement = encodePlacement(game.board)
    val side = game.sideToMove match
      case Color.White => "w"
      case Color.Black => "b"
    s"$placement $side"

