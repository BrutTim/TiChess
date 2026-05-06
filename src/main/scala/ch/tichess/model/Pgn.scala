package ch.tichess.model

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import fastparse.*
import fastparse.NoWhitespace.*

trait PgnParser:
  def name: String
  def parse(pgn: String, fenParser: FenParser): Either[String, ImportedPgn]

final case class ParserChoice(id: String, label: String, fenParser: FenParser, pgnParser: PgnParser)
final case class ImportedPgn(startGame: Game, moves: Vector[Move], game: Game, result: String)

private[ch] final case class RawPgn(tags: Map[String, String], tokens: Vector[String])

private[ch] object PgnSupport:
  private val supportedResults = Set("*", "1-0", "0-1", "1/2-1/2")
  private val moveNumberPattern = raw"\d+\.+".r
  private val moveTokenPattern = raw"([a-h][1-8])[-]?([a-h][1-8])(?:=?([qrbnQRBN]))?".r

  def encode(startGame: Game, moves: Vector[Move], result: Option[String] = None): String =
    val effectiveResult = result.getOrElse(inferResult(startGame, moves))
    val today = java.time.LocalDate.now()
    val baseTags = Vector(
      "Event" -> "TiChess Game",
      "Site" -> "Local",
      "Date" -> today.format(java.time.format.DateTimeFormatter.ofPattern("yyyy.MM.dd")),
      "Round" -> "-",
      "White" -> "White",
      "Black" -> "Black",
      "Result" -> effectiveResult
    )
    val setupTags =
      if startGame == Game.initial then Vector.empty
      else Vector("SetUp" -> "1", "FEN" -> Fen.encode(startGame))

    val allTags = baseTags ++ setupTags
    val sb = new java.lang.StringBuilder()
    var i = 0
    while i < allTags.length do
      val (key, value) = allTags(i)
      sb.append("[").append(key).append(" \"").append(escape(value)).append("\"]\n")
      i += 1
    sb.append("\n").append(renderMoves(moves, effectiveResult))
    sb.toString

  def buildImportedPgn(raw: RawPgn, fenParser: FenParser): Either[String, ImportedPgn] =
    for
      result <- extractResult(raw.tags, raw.tokens)
      startGame <- extractStartGame(raw.tags, fenParser)
      moveTokens <- normalizeMoveTokens(raw.tokens, result)
      moves <- moveTokens.foldLeft(Right(Vector.empty): Either[String, Vector[Move]]) { (accE, token) =>
        for
          acc <- accE
          move <- parseMoveToken(token)
        yield acc :+ move
      }
      finalGame <- replayMoves(startGame, moves)
    yield ImportedPgn(startGame, moves, finalGame, result)

  private def replayMoves(startGame: Game, moves: Vector[Move]): Either[String, Game] =
    moves.foldLeft(Right(startGame): Either[String, Game]) { (gameE, move) =>
      gameE.flatMap(_.applyMove(move))
    }

  private def inferResult(startGame: Game, moves: Vector[Move]): String =
    replayMoves(startGame, moves) match
      case Right(finalGame) if finalGame.isCheckmate =>
        finalGame.sideToMove.other match
          case Color.White => "1-0"
          case Color.Black => "0-1"
      case Right(finalGame) if finalGame.isDraw => "1/2-1/2"
      case _ =>
        "*"

  private def extractStartGame(tags: Map[String, String], fenParser: FenParser): Either[String, Game] =
    val maybeFen = tags.get("FEN").orElse(tags.get("Fen"))
    val maybeSetup = tags.get("SetUp").orElse(tags.get("Setup"))

    maybeFen match
      case Some(fen) =>
        maybeSetup match
          case Some("1") | None => fenParser.parse(fen)
          case Some(_) => Left("""PGN SetUp tag must be "1" when FEN is present.""")
      case None =>
        Right(Game.initial)

  private def extractResult(tags: Map[String, String], tokens: Vector[String]): Either[String, String] =
    val tagResult = tags.get("Result")
    val trailingResult = tokens.lastOption.filter(supportedResults.contains)

    val result =
      trailingResult
        .orElse(tagResult)
        .getOrElse("*")

    if supportedResults.contains(result) then Right(result)
    else Left("PGN result must be one of *, 1-0, 0-1, or 1/2-1/2.")

  private def normalizeMoveTokens(tokens: Vector[String], result: String): Either[String, Vector[String]] =
    val trimmed =
      if tokens.lastOption.contains(result) then tokens.dropRight(1) else tokens

    trimmed.foldLeft(Right(Vector.empty): Either[String, Vector[String]]) { (accE, token) =>
      accE.flatMap { acc =>
        if moveNumberPattern.matches(token) then Right(acc)
        else if moveTokenPattern.matches(token) then Right(acc :+ token)
        else Left(s"Unsupported PGN movetext token: $token")
      }
    }

  private[model] def parseMoveToken(token: String): Either[String, Move] =
    token match
      case moveTokenPattern(from, to, promotionRaw) =>
        for
          fromPos <- Pos.fromAlgebraic(from)
          toPos <- Pos.fromAlgebraic(to)
          promotion <- parsePromotion(promotionRaw)
        yield Move(fromPos, toPos, promotion)
      case _ =>
        Left(s"Unsupported PGN movetext token: $token")

  private def parsePromotion(raw: String | Null): Either[String, Option[PromotionRole]] =
    Option(raw) match
      case None => Right(None)
      case Some(value) => PromotionRole.fromPromotionChar(value).map(Some(_))

  private def renderMoves(moves: Vector[Move], result: String): String =
    if moves.isEmpty then return result
    val sb = new java.lang.StringBuilder()
    var i = 0
    var moveNum = 1
    val len = moves.length
    while i < len do
      if i > 0 then sb.append(" ")
      sb.append(moveNum).append(". ").append(renderMove(moves(i)))
      if i + 1 < len then
        sb.append(" ").append(renderMove(moves(i + 1)))
      i += 2
      moveNum += 1
    sb.append(" ").append(result)
    sb.toString

  private def renderMove(move: Move): String =
    val promotion = move.promotion.map {
      case PromotionRole.Queen  => "=Q"
      case PromotionRole.Rook   => "=R"
      case PromotionRole.Bishop => "=B"
      case PromotionRole.Knight => "=N"
    }.getOrElse("")
    s"${toAlg(move.from)}${toAlg(move.to)}$promotion"

  private def toAlg(pos: Pos): String =
    s"${('a' + pos.file).toChar}${pos.rank + 1}"

  private def escape(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"")

object ParserCombinatorsPgnParser extends RegexParsers with PgnParser:
  override val skipWhitespace: Boolean = false
  override val name: String = "scala-parser-combinators"

  private def wsChar: Parser[String] = " " | "\t" | "\r" | "\n"
  private def optWs: Parser[Unit] = rep(wsChar) ^^ (_ => ())
  private def ws: Parser[String] = rep1(wsChar) ^^ (_.mkString)
  private def tokenChar: Parser[String] = """[^ \t\r\n\[\]]""".r
  private def token: Parser[String] = rep1(tokenChar) ^^ (_.mkString)
  private def tagName: Parser[String] = """[A-Za-z][A-Za-z0-9_]*""".r
  private def quotedText: Parser[String] = "\"([^\"]|\\\\\"|\\\\\\\\)*\"".r ^^ { raw =>
    raw.substring(1, raw.length - 1).replace("\\\"", "\"").replace("\\\\", "\\")
  }
  private def tag: Parser[(String, String)] =
    "[" ~> tagName ~ ws ~ quotedText <~ optWs <~ "]" ^^ { case name ~ _ ~ value => name -> value }
  private def rawPgn: Parser[RawPgn] =
    optWs ~> rep(tag <~ optWs) ~ rep(token <~ optWs) ^^ { case tags ~ tokens => RawPgn(tags.toMap, tokens.toVector) }

  def parse(pgn: String, fenParser: FenParser): Either[String, ImportedPgn] =
    parseAll(rawPgn, pgn) match
      case Success(raw, _) =>
        PgnSupport.buildImportedPgn(raw, fenParser).left.map {
          case err if err.startsWith("Unsupported PGN movetext token:") => "Invalid PGN movetext."
          case err => err
        }
      case _ => Left("Invalid PGN format for a game-state import.")


object RegexPgnParser extends PgnParser:
  override val name: String = "regex/manual"

  private val tagPattern: Regex = raw"""\[([A-Za-z][A-Za-z0-9_]*)\s+"([^"]*)"\]""".r
  private val tokenPattern: Regex = raw"""[^\s\[\]]+""".r

  def parse(pgn: String, fenParser: FenParser): Either[String, ImportedPgn] =
    val lines = pgn.linesIterator.toList

    @annotation.tailrec
    def collectTags(rest: List[String], acc: Vector[(String, String)]): Either[String, (Vector[(String, String)], List[String])] =
      rest match
        case line :: tail if line.trim.isEmpty =>
          if acc.nonEmpty then Right((acc, tail)) else collectTags(tail, acc)
        case line :: tail =>
          line.trim match
            case tagPattern(name, value) => collectTags(tail, acc :+ (name -> value))
            case _ => Right((acc, rest))
        case Nil => Right((acc, Nil))

    for
      parsed <- collectTags(lines, Vector.empty)
      (tags, remaining) = parsed
      tokens = remaining.flatMap(line => tokenPattern.findAllIn(line)).toVector
      imported <- PgnSupport.buildImportedPgn(RawPgn(tags.toMap, tokens), fenParser).left.map {
        case err if err.startsWith("Unsupported PGN movetext token:") => "Invalid PGN movetext."
        case err => err
      }
    yield imported

object NotationParsers:
  val combinators: ParserChoice =
    ParserChoice("combinators", "Parser Combinators", ParserCombinatorsFenParser, ParserCombinatorsPgnParser)
  val fastparse: ParserChoice =
    ParserChoice("fastparse", "FastParse", FastParseFenParser, FastParsePgnParser)
  val regex: ParserChoice =
    ParserChoice("regex", "Regex", RegexFenParser, RegexPgnParser)

  val all: List[ParserChoice] = List(fastparse, combinators, regex)
  val default: ParserChoice = fastparse

  def ids: List[String] = all.map(_.id)

  def resolve(input: String): Either[String, ParserChoice] =
    val normalized = input.trim.toLowerCase
    all.find(choice => choice.id == normalized || choice.label.toLowerCase == normalized) match
      case Some(choice) => Right(choice)
      case None => Left(s"Unknown parser '$input'. Available parsers: ${ids.mkString(", ")}.")

object Pgn:
  def encode(startGame: Game, moves: Vector[Move], result: Option[String] = None): String =
    PgnSupport.encode(startGame, moves, result)

  def parse(pgn: String, parserChoice: ParserChoice = NotationParsers.default): Either[String, ImportedPgn] =
    parserChoice.pgnParser.parse(pgn, parserChoice.fenParser)
