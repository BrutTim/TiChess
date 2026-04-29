package ch.tichess.controller.persistence

import ch.tichess.model.{Fen, Game, Move, Pos, PromotionRole}

import scala.io.Source

object LichessPuzzleImporter {
  def fromCsvFile(path: String, limit: Int = 200): Seq[ChallengeRecord] =
    val source = Source.fromFile(path)
    try fromCsvRows(source.getLines().drop(1).toSeq, limit)
    finally source.close()

  def fromCsvRows(rows: Seq[String], limit: Int = 200): Seq[ChallengeRecord] =
    rows.iterator.flatMap(parseRow).take(limit).toSeq

  private def parseRow(row: String): Option[ChallengeRecord] =
    val columns = row.split(",", -1)
    if columns.length < 3 then None
    else
      val id = columns(0).trim
      val fen = columns(1).trim
      val uciMoves = columns(2).trim.split("\\s+").toVector.filter(_.nonEmpty)

      for
        game <- Fen.parse(fen).toOption
        firstMove <- uciMoves.headOption.flatMap(parseUciMove)
        shownGame <- game.applyMove(firstMove).toOption
        solutionMoves = uciMoves.tail.flatMap(parseUciMove).map(displayMove)
        if solutionMoves.nonEmpty
      yield ChallengeRecord(
        id = id,
        name = s"Lichess $id",
        fen = Fen.encode(shownGame),
        moves = solutionMoves.mkString(", ")
      )

  private def parseUciMove(uci: String): Option[Move] =
    if uci.length != 4 && uci.length != 5 then None
    else
      for
        from <- Pos.fromAlgebraic(uci.substring(0, 2)).toOption
        to <- Pos.fromAlgebraic(uci.substring(2, 4)).toOption
        promotion <- parsePromotion(uci.drop(4))
      yield Move(from, to, promotion)

  private def parsePromotion(suffix: String): Option[Option[PromotionRole]] =
    if suffix.isEmpty then Some(None)
    else PromotionRole.fromPromotionChar(suffix).toOption.map(Some(_))

  private def displayMove(move: Move): String =
    val promotion = move.promotion.map {
      case PromotionRole.Queen  => " q"
      case PromotionRole.Rook   => " r"
      case PromotionRole.Bishop => " b"
      case PromotionRole.Knight => " n"
    }.getOrElse("")
    s"${move.from.toAlgebraic} ${move.to.toAlgebraic}$promotion"
}
