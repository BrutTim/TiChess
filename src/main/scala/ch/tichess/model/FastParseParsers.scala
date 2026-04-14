package ch.tichess.model

import fastparse.*
import fastparse.NoWhitespace.*

// $COVERAGE-OFF$
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


object FastParsePgnParser extends PgnParser:
  override val name: String = "FastParse"

  private def optWs[$: P]: P[Unit] = P(CharsWhileIn(" \t\r\n").rep)
  private def ws[$: P]: P[Unit] = P(CharsWhileIn(" \t\r\n").rep(1))
  private def tagName[$: P]: P[String] = P(CharIn("A-Za-z") ~ CharsWhileIn("A-Za-z0-9_", 0)).!
  private def quotedText[$: P]: P[String] =
    P("\"" ~/ CharsWhile(c => c != '"' && c != '\n' && c != '\r').! ~ "\"")
  private def tag[$: P]: P[(String, String)] =
    P("[" ~/ tagName ~ ws ~ quotedText ~ "]").map((name, value) => name -> value)
  private def tagEntry[$: P]: P[(String, String)] =
    P(tag ~ optWs).map((name, value) => name -> value)
  private def token[$: P]: P[String] = P(CharsWhile(c => !c.isWhitespace && c != '[' && c != ']', min = 1).!)
  private def rawPgn[$: P]: P[RawPgn] =
    P(optWs ~ tagEntry.rep ~ token.rep(sep = ws) ~ optWs ~ End).map { case (tags, tokens) =>
      RawPgn(tags.toMap, tokens.toVector)
    }

  def parse(pgn: String, fenParser: FenParser): Either[String, ImportedPgn] =
    fastparse.parse(pgn, rawPgn(_)) match
      case Parsed.Success(raw, _) =>
        PgnSupport.buildImportedPgn(raw, fenParser).left.map {
          case err if err.startsWith("Unsupported PGN movetext token:") => "Invalid PGN movetext."
          case err => err
        }
      case _: Parsed.Failure => Left("Invalid PGN format for a game-state import.")
// $COVERAGE-ON$
