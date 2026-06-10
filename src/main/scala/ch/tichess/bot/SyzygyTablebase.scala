package ch.tichess.bot

import ch.tichess.model.{Game, Move, Pos, PromotionRole}
import ch.tichess.model.Fen

import java.io.File
import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

final class SyzygyTablebase(
    tablebasePath: String,
    pythonCommand: String = SyzygyTablebase.defaultPythonCommand,
    scriptPath: String = "src/main/python/syzygy_probe.py",
    maxPieces: Int = 5
):
  import SyzygyTablebase.ProbeResult

  private val probeCache: mutable.HashMap[String, ProbeResult] = mutable.HashMap.empty

  def bestMove(game: Game): Option[Move] =
    probe(game).map(_.bestMove)

  def probe(game: Game): Option[ProbeResult] =
    if game.board.allPieces.size > maxPieces then None
    else if !File(tablebasePath).isDirectory then None
    else
      val fen = Fen.encode(game)
      probeCache.get(fen).orElse {
        val result = runProbe(fen)
        result.foreach(probeCache.update(fen, _))
        result
      }

  def canProbe(game: Game): Boolean =
    game.board.allPieces.size <= maxPieces && File(tablebasePath).isDirectory

  private def runProbe(fen: String): Option[ProbeResult] =
      val output = new StringBuilder
      val errors = new StringBuilder
      val command = Seq(pythonCommand, scriptPath, tablebasePath, fen)
      val exitCode =
        Try {
          Process(command).!(ProcessLogger(
            line => output.append(line).append('\n'),
            line => errors.append(line).append('\n')
          ))
        }.getOrElse(-1)

      if exitCode == 0 then parseProbeOutput(output.toString)
      else None

  private def parseProbeOutput(output: String): Option[ProbeResult] =
    val lines = output.linesIterator.map(_.trim).toList
    val evaluatedMoves = lines.flatMap(parseMoveEvaluation)
    for
      moveLine <- lines.find(_.startsWith("bestmove "))
      bestMove <- parseUciMove(moveLine.stripPrefix("bestmove ").trim)
      wdlLine <- lines.find(_.startsWith("wdl "))
      wdl <- wdlLine.stripPrefix("wdl ").trim.toIntOption
      dtzLine <- lines.find(_.startsWith("dtz "))
      dtz <- dtzLine.stripPrefix("dtz ").trim.toIntOption
    yield ProbeResult(bestMove, wdl, dtz, evaluatedMoves)

  private def parseMoveEvaluation(line: String): Option[SyzygyTablebase.MoveEvaluation] =
    val parts = line.split("\\s+").toList
    parts match
      case "move" :: uci :: "wdl" :: wdlText :: "dtz" :: dtzText :: Nil =>
        for
          move <- parseUciMove(uci)
          wdl <- wdlText.toIntOption
          dtz <- dtzText.toIntOption
        yield SyzygyTablebase.MoveEvaluation(move, wdl, dtz)
      case _ => None

  private def parseUciMove(uci: String): Option[Move] =
    if uci.length < 4 then None
    else
      for
        from <- Pos.fromAlgebraic(uci.substring(0, 2)).toOption
        to <- Pos.fromAlgebraic(uci.substring(2, 4)).toOption
        promotion <- parsePromotion(uci.drop(4))
      yield Move(from, to, promotion)

  private def parsePromotion(s: String): Option[Option[PromotionRole]] =
    if s.isEmpty then Some(None)
    else PromotionRole.fromPromotionChar(s).toOption.map(Some(_))

object SyzygyTablebase:
  final case class MoveEvaluation(move: Move, wdl: Int, dtz: Int)

  final case class ProbeResult(bestMove: Move, wdl: Int, dtz: Int, moves: List[MoveEvaluation] = Nil):
    def label: String =
      val wdlText =
        if wdl > 0 then "win"
        else if wdl < 0 then "loss"
        else "draw"
      s"$wdlText wdl=$wdl dtz=$dtz"

  private val localDefaultPath = "src/main/resources/3-4-5_pieces_Syzygy/3-4-5"

  def fromEnv(): Option[SyzygyTablebase] =
    fromConfiguredPath(
      sys.env.get("SYZYGY_PATH"),
      localDefaultAvailable = File(localDefaultPath).isDirectory,
      sys.env.get("TICHESS_PYTHON"),
      sys.env.get("SYZYGY_SCRIPT")
    )

  private[bot] def fromConfiguredPath(
      configuredSyzygyPath: Option[String],
      localDefaultAvailable: Boolean,
      configuredPython: Option[String],
      configuredScript: Option[String] = None
  ): Option[SyzygyTablebase] =
    val configuredPath =
      configuredSyzygyPath.filter(_.nonEmpty)
        .orElse(Option.when(localDefaultAvailable)(localDefaultPath))

    configuredPath.map { path =>
      SyzygyTablebase(
        path,
        configuredPython.getOrElse(defaultPythonCommand),
        configuredScript.filter(_.nonEmpty).getOrElse("src/main/python/syzygy_probe.py")
      )
    }

  private def defaultPythonCommand: String =
    defaultPythonCommandFor(File("venv/bin/python"))

  private[bot] def defaultPythonCommandFor(venvPython: File): String =
    if venvPython.isFile then venvPython.getPath else "python3"
