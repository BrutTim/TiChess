package ch.tichess.controller

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class ControllerSpec extends AnyFunSuite:

  test("Command.parse handles quit/help and empty") {
    assert(Command.parse("quit") == Right(Command.Quit))
    assert(Command.parse("Q") == Right(Command.Quit))
    assert(Command.parse("help") == Right(Command.Help))
    assert(Command.parse("  ") == Left("Empty input."))
  }

  test("Command.parse parses move and rejects bad format") {
    assert(Command.parse("e2 e4").isRight)
    assert(Command.parse("e2").isLeft)
    assert(Command.parse("e2 e9").isLeft)
    assert(Command.parse("e2  e4  ").isRight)
  }

  test("Command.parse handles fen command and rejects missing payload") {
    assert(Command.parse("fen").left.exists(_.contains("Expected a FEN")))
    assert(Command.parse("fen   ").left.exists(_.contains("Expected a FEN")))
    assert(Command.parse("fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w").isRight)
  }

  test("Controller.update returns messages and updates game") {
    val g0 = Controller.initial

    val empty = Controller.update(g0, "   ")
    assert(empty.game == g0)
    assert(empty.message.contains("Empty input."))
    assert(!empty.quit)

    val bad = Controller.update(g0, "e2 e5")
    assert(bad.game == g0)
    assert(bad.message.exists(_.nonEmpty))
    assert(!bad.quit)

    val ok = Controller.update(g0, "e2 e4")
    assert(ok.game.sideToMove == Color.Black)
    assert(ok.message.isEmpty)
    assert(!ok.quit)

    val help = Controller.update(g0, "help")
    assert(help.game == g0)
    assert(help.message.contains(List(
                                      "- Zug eingeben: `e2 e4`",
                                      "- Hilfe anzeigen: `help`",
                                      "- Spiel beenden: `quit`",
                                      "- Position setzen (FEN, minimal): `fen <placement> <w|b>`",
                                      "- Beispiel: `fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w`"
                                    ).mkString("\n")))
    assert(!help.quit)

    val quit = Controller.update(g0, "quit")
    assert(quit.game == g0)
    assert(quit.message.contains("Bye."))
    assert(quit.quit)
  }

  test("Controller.update ends game with black winner on checkmate") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(0, 0) -> Piece(Color.White, PieceType.King), // a1
        Pos(2, 1) -> Piece(Color.Black, PieceType.Queen), // c2
        Pos(2, 2) -> Piece(Color.Black, PieceType.King) // c3
      )
    )
    val game = Game(board, Color.Black)

    // Black plays Qb2#, white king on a1 has no legal escape.
    val res = Controller.update(game, "c2 b2")
    assert(res.quit)
    assert(res.message.contains("Checkmate. Black wins."))
    assert(res.game.sideToMove == Color.White)
    assert(res.game.isCheckmate)
  }

  test("Controller.update sets position from FEN without showing FEN in output") {
    val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"
    val res = Controller.update(Controller.initial, s"fen $fen")
    assert(!res.quit)
    assert(res.message.contains("Position set."))
    assert(res.game == Fen.parse(fen).toOption.get)
  }

  test("Controller.update ends game when set FEN is already checkmate") {
    val mateFen = "k7/1Q6/2K5/8/8/8/8/8 b"
    val res = Controller.update(Controller.initial, s"fen $mateFen")
    assert(res.quit)
    assert(res.message.contains("Checkmate. White wins."))
    assert(res.game.isCheckmate)
    assert(res.game.sideToMove == Color.Black)
  }

  test("Controller.update returns error when FEN parsing fails") {
    val res = Controller.update(Controller.initial, "fen 8/8/8/8/8/8/8/4K3 x")
    assert(!res.quit)
    assert(res.game == Controller.initial)
    assert(res.message.exists(_.contains("FEN side-to-move must be 'w' or 'b'.")))
  }

