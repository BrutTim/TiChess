package ch.tichess.model

import scala.util.Using
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import fastparse.*
import fastparse.NoWhitespace.*

trait FenParser:
  def name: String
  def parse(fen: String): Either[String, Game]

private[ch] final case class RawFen(placement: String, side: String)

private[ch] object FenSupport:
  val FieldError = "FEN must contain exactly placement and side-to-move."

  def pieceToChar(piece: Piece): Char =
    val base = piece.kind match
      case PieceType.King => 'K'
      case PieceType.Queen => 'Q'
      case PieceType.Rook => 'R'
      case PieceType.Bishop => 'B'
      case PieceType.Knight => 'N'
      case PieceType.Pawn => 'P'

    piece.color match
      case Color.White => base
      case Color.Black => base.toLower

  def buildGame(raw: RawFen): Either[String, Game] =
    val sideToMove =
      raw.side.toLowerCase match
        case "w" => Right(Color.White)
        case "b" => Right(Color.Black)
        case _ => Left("FEN side-to-move must be 'w' or 'b'.")

    for
      side <- sideToMove
      board <- parsePlacement(raw.placement)
      _ <- validateKings(board)
    yield Game(board, side)

  def encode(game: Game): String =
    val placement = encodePlacement(game.board)
    val side = game.sideToMove match
      case Color.White => "w"
      case Color.Black => "b"
    s"$placement $side"

  def parseFile(path: String, parser: FenParser): Either[String, Game] =
    for
      fen <- Using(scala.io.Source.fromFile(path))(_.mkString).toEither.left.map(err =>
        s"Could not read FEN file: ${err.getMessage}"
      )
      game <- parser.parse(fen)
    yield game

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

        val endState =
          rankStr.foldLeft(init) { (stateE, ch) =>
            stateE.flatMap { (file, acc) =>
              if ch.isDigit then
                val empty = ch.asDigit
                if empty < 1 || empty > 8 then Left("FEN digit must be 1..8.")
                else if file + empty > 8 then Left("FEN rank has too many squares.")
                else Right((file + empty, acc))
              else if file > 7 then Left("FEN rank has too many squares.")
              else
                for
                  piece <- charToPiece(ch)
                  pos = Pos(file, rank)
                yield (file + 1, acc + (pos -> piece))
            }
          }

        endState.flatMap { (file, acc) =>
          if file != 8 then Left("FEN rank does not cover exactly 8 squares.")
          else Right(acc)
        }

      rows.zipWithIndex
        .foldLeft(Right(Map.empty): Either[String, Map[Pos, Piece]]) {
          case (accE, (rankStr, rowIdx)) =>
            accE.flatMap { acc =>
              val rank = 7 - rowIdx
              parseRank(rankStr, rank, acc)
            }
        }
        .map(Board.apply)

  private def encodePlacement(board: Board): String =
    def rowString(rank: Int): String =
      val (empties, parts) =
        (0 until 8).foldLeft((0, List.empty[String])) {
          case ((emptyCount, acc), file) =>
            board.pieceAt(Pos(file, rank)) match
              case None => (emptyCount + 1, acc)
              case Some(piece) =>
                val withEmpties =
                  if emptyCount > 0 then acc :+ emptyCount.toString else acc
                (0, withEmpties :+ pieceToChar(piece).toString)
        }

      if empties > 0 then (parts :+ empties.toString).mkString else parts.mkString

    (7 to 0 by -1).map(rowString).mkString("/")

  private def validateKings(board: Board): Either[String, Unit] =
    val whiteKings = board.allPieces.values.count(p => p.kind == PieceType.King && p.color == Color.White)
    val blackKings = board.allPieces.values.count(p => p.kind == PieceType.King && p.color == Color.Black)

    if whiteKings != 1 then Left("FEN must contain exactly one white king.")
    else if blackKings != 1 then Left("FEN must contain exactly one black king.")
    else Right(())

object ParserCombinatorsFenParser extends RegexParsers with FenParser:
  override val skipWhitespace: Boolean = false

  override val name: String = "scala-parser-combinators"

  private def wsChar: Parser[String] = " " | "\t" | "\r" | "\n"
  private def ws: Parser[String] = rep1(wsChar) ^^ (_.mkString)
  private def optWs: Parser[Unit] = rep(wsChar) ^^ (_ => ())
  private def tokenChar: Parser[String] = """[^ \t\r\n]""".r
  private def token: Parser[String] = rep1(tokenChar) ^^ (_.mkString)

  private def rawFen: Parser[RawFen] =
    optWs ~> token ~ ws ~ token <~ optWs ^^ { case placement ~ _ ~ side => RawFen(placement, side) }

  def parse(fen: String): Either[String, Game] =
    parseAll(rawFen, fen) match
      case Success(raw, _) => FenSupport.buildGame(raw)
      case _ => Left(FenSupport.FieldError)

object FastParseFenParser extends FenParser:
  override val name: String = "FastParse"

  private def rawFen[$: P]: P[RawFen] =
    P(
      CharsWhileIn(" \t\r\n").rep ~
        CharsWhile(ch => !ch.isWhitespace, min = 1).! ~
        CharsWhileIn(" \t\r\n").rep(1) ~
        CharsWhile(ch => !ch.isWhitespace, min = 1).! ~
        CharsWhileIn(" \t\r\n").rep ~
        End
    ).map { case (placement, side) => RawFen(placement, side) }

  def parse(fen: String): Either[String, Game] =
    fastparse.parse(fen, rawFen(_)) match
      case Parsed.Success(raw, _) => FenSupport.buildGame(raw)
      case _: Parsed.Failure => Left(FenSupport.FieldError)

object RegexFenParser extends FenParser:
  override val name: String = "regex/manual"

  private val tokenPattern: Regex = raw"\S+".r

  def parse(fen: String): Either[String, Game] =
    val tokens =
      fen
        .linesIterator
        .flatMap(line => tokenPattern.findAllIn(line))
        .toList

    tokens match
      case placement :: side :: Nil => FenSupport.buildGame(RawFen(placement, side))
      case _ => Left(FenSupport.FieldError)

object FenParsers:
  val all: List[FenParser] = List(ParserCombinatorsFenParser, FastParseFenParser, RegexFenParser)

object Fen:
  val defaultParser: FenParser = FastParseFenParser

  def parse(fen: String): Either[String, Game] = defaultParser.parse(fen)

  def parseWith(parser: FenParser, fen: String): Either[String, Game] = parser.parse(fen)

  def parseFile(path: String): Either[String, Game] = FenSupport.parseFile(path, defaultParser)

  def parseFileWith(parser: FenParser, path: String): Either[String, Game] =
    FenSupport.parseFile(path, parser)

  def encode(game: Game): String = FenSupport.encode(game)
