package ch.tichess.bot

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files
import java.io.File
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

final class BotCoverageSpec extends AnyFunSuite:

  test("OpeningMoveStats score rewards stronger and better supported moves") {
    val move = Move(Pos(4, 1), Pos(4, 3))
    val weak = OpeningMoveStats(move, played = 1, wins = 1, draws = 0, losses = 0)
    val strong = OpeningMoveStats(move, played = 100, wins = 70, draws = 20, losses = 10)

    assert(strong.score > weak.score)
  }

  test("PgnOpeningDatabase records outcomes and loads valid PGNs while skipping invalid games") {
    val db = PgnOpeningDatabase()
    val startFen = Fen.encodeNormalized(Game.initial)
    val e4 = Move(Pos(4, 1), Pos(4, 3))

    assert(Await.result(db.getMoves("unknown fen"), 2.seconds).isEmpty)

    Await.result(db.recordOutcome(startFen, e4, 1), 2.seconds)
    Await.result(db.recordOutcome(startFen, e4, 0), 2.seconds)
    Await.result(db.recordOutcome(startFen, e4, -1), 2.seconds)
    Await.result(db.recordOutcome(startFen, e4, 99), 2.seconds)

    val stats = Await.result(db.getMoves(startFen), 2.seconds).find(_.move == e4).get
    assert(stats.played == 4)
    assert(stats.wins == 1)
    assert(stats.draws == 1)
    assert(stats.losses == 1)

    val pgn =
      """[Event "Mini"]
        |[Site "?"]
        |[Date "2026.05.06"]
        |[Round "?"]
        |[White "White"]
        |[Black "Black"]
        |[Result "1-0"]
        |
        |1. e2e4 e7e5 1-0
        |
        |[Event "Broken"]
        |
        |not a game
        |""".stripMargin

    val loaded = db.loadFromPgnString(pgn.replace("\n", "\r\n"), maxMovesPerGame = 1)
    assert(loaded == 1)
    assert(db.positionCount >= 1)

    val unclearResultPgn =
      """[Event "Unclear"]
        |[Site "?"]
        |[Date "2026.05.06"]
        |[Round "?"]
        |[White "White"]
        |[Black "Black"]
        |[Result "*"]
        |
        |1. e2e4 *
        |""".stripMargin
    assert(db.loadFromPgnString(unclearResultPgn, maxMovesPerGame = 1) == 1)
  }

  test("ZobristHash changes for side, castling, en-passant and halfmove state") {
    val initial = Game.initial
    val blackToMove = initial.copy(sideToMove = Color.Black)
    val noCastling = initial.copy(castlingRights = CastlingRights.empty)
    val enPassant = initial.copy(enPassantTarget = Some(Pos(4, 2)))
    val highHalfmove = initial.copy(halfMoveClock = 150)

    val initialHash = ZobristHash.hash(initial)
    assert(ZobristHash.hash(blackToMove) != initialHash)
    assert(ZobristHash.hash(noCastling) != initialHash)
    assert(ZobristHash.hash(enPassant) != initialHash)
    assert(ZobristHash.hash(highHalfmove) != initialHash)
    assert(ZobristHash.hash(initial) == initialHash)
  }

  test("SyzygyTablebase handles unavailable paths, probe output, caching and labels") {
    val missing = SyzygyTablebase("/definitely/missing/syzygy")
    assert(!missing.canProbe(Game.initial))
    assert(missing.probe(Game.initial).isEmpty)
    assert(missing.bestMove(Game.initial).isEmpty)

    val smallBoard = Board.empty.copy(
      pieces = Map(
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(4, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val smallGame = Game(smallBoard, Color.White)
    assert(missing.probe(smallGame).isEmpty)

    val tableDir = Files.createTempDirectory("syzygy-test")
    val probeScript = Files.createTempFile("syzygy-probe", ".sh")
    Files.writeString(
      probeScript,
      "#!/bin/sh\nprintf 'bestmove a7a8q\\nwdl 2\\ndtz 5\\n'\n"
    )
    probeScript.toFile.setExecutable(true)

    val board = Board.empty.copy(
      pieces = Map(
        Pos(0, 6) -> Piece(Color.White, PieceType.Pawn),
        Pos(4, 0) -> Piece(Color.White, PieceType.King),
        Pos(4, 7) -> Piece(Color.Black, PieceType.King)
      )
    )
    val game = Game(board, Color.White)
    val tablebase = SyzygyTablebase(tableDir.toString, probeScript.toString, scriptPath = "ignored", maxPieces = 3)

    assert(tablebase.canProbe(game))
    val result = tablebase.probe(game).get
    assert(result.bestMove == Move(Pos(0, 6), Pos(0, 7), Some(PromotionRole.Queen)))
    assert(result.label == "win wdl=2 dtz=5")
    assert(tablebase.bestMove(game).contains(result.bestMove))
    assert(SyzygyTablebase.ProbeResult(result.bestMove, -1, 3).label == "loss wdl=-1 dtz=3")
    assert(SyzygyTablebase.ProbeResult(result.bestMove, 0, 0).label == "draw wdl=0 dtz=0")

    val noPromotionScript = Files.createTempFile("syzygy-probe-no-promo", ".sh")
    Files.writeString(
      noPromotionScript,
      "#!/bin/sh\nprintf 'bestmove e1e2\\nwdl 0\\ndtz 0\\n'\n"
    )
    noPromotionScript.toFile.setExecutable(true)
    val noPromotion = SyzygyTablebase(tableDir.toString, noPromotionScript.toString, scriptPath = "ignored", maxPieces = 3)
    assert(noPromotion.probe(smallGame).exists(_.bestMove == Move(Pos(4, 0), Pos(4, 1))))

    val stderrScript = Files.createTempFile("syzygy-probe-stderr", ".sh")
    Files.writeString(
      stderrScript,
      "#!/bin/sh\nprintf 'noise\\n' >&2\nprintf 'bestmove e1e2\\nwdl 0\\ndtz 0\\n'\n"
    )
    stderrScript.toFile.setExecutable(true)
    val stderrProbe = SyzygyTablebase(tableDir.toString, stderrScript.toString, scriptPath = "ignored", maxPieces = 3)
    assert(stderrProbe.probe(smallGame).nonEmpty)

    val failingScript = Files.createTempFile("syzygy-probe-failing", ".sh")
    Files.writeString(failingScript, "#!/bin/sh\nexit 7\n")
    failingScript.toFile.setExecutable(true)
    val failingProbe = SyzygyTablebase(tableDir.toString, failingScript.toString, scriptPath = "ignored", maxPieces = 3)
    assert(failingProbe.probe(smallGame).isEmpty)

    val invalidMoveScript = Files.createTempFile("syzygy-probe-invalid", ".sh")
    Files.writeString(
      invalidMoveScript,
      "#!/bin/sh\nprintf 'bestmove e1\\nwdl 0\\ndtz 0\\n'\n"
    )
    invalidMoveScript.toFile.setExecutable(true)
    val invalidProbe = SyzygyTablebase(tableDir.toString, invalidMoveScript.toString, scriptPath = "ignored", maxPieces = 3)
    assert(invalidProbe.probe(smallGame).isEmpty)
  }

  test("SyzygyTablebase configuration helpers cover env and default python choices") {
    assert(SyzygyTablebase.fromConfiguredPath(None, localDefaultAvailable = false, None).isEmpty)
    assert(SyzygyTablebase.fromConfiguredPath(Some(""), localDefaultAvailable = false, None).isEmpty)
    assert(SyzygyTablebase.fromConfiguredPath(Some("/tmp/syzygy"), localDefaultAvailable = false, Some("python-custom")).nonEmpty)
    assert(SyzygyTablebase.fromConfiguredPath(None, localDefaultAvailable = true, None).nonEmpty)

    val python = Files.createTempFile("python", ".bin").toFile
    assert(SyzygyTablebase.defaultPythonCommandFor(python) == python.getPath)
    assert(SyzygyTablebase.defaultPythonCommandFor(File("/definitely/missing/python")) == "python3")
  }
