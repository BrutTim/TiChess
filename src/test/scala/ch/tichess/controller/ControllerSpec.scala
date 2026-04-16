package ch.tichess.controller

import ch.tichess.model.*
import org.scalatest.funsuite.AnyFunSuite

final class ControllerSpec extends AnyFunSuite:

  test("Command.parse handles quit/help and empty") {
    assert(Command.parse("quit") == Right(Command.Quit))
    assert(Command.parse("Q") == Right(Command.Quit))
    assert(Command.parse("help") == Right(Command.Help))
    assert(Command.parse("parser") == Right(Command.ShowParserCmd))
    assert(Command.parse("  ") == Left("Empty input."))
  }

  test("Command.parse parses move and rejects bad format") {
    assert(Command.parse("e2 e4").isRight)
    assert(Command.parse("e7 e8 q") == Right(Command.MoveCmd(Move(Pos(4, 6), Pos(4, 7), Some(PromotionRole.Queen)))))
    assert(Command.parse("e7 e8 k") == Left("Promotion must be one of: q, r, b, n."))
    assert(Command.parse("e2").isLeft)
    assert(Command.parse("e2 e9").isLeft)
    assert(Command.parse("e2  e4  ").isRight)
  }

  test("Command.parse handles fen command and rejects missing payload") {
    assert(Command.parse("fen").left.exists(_.contains("Expected a FEN")))
    assert(Command.parse("fen   ").left.exists(_.contains("Expected a FEN")))
    assert(Command.parse("pgn").left.exists(_.contains("Expected a PGN")))
    assert(Command.parse("fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1") == Right(Command.ImportFenCmd("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")))
    assert(Command.parse("fen import rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1") == Right(Command.ImportFenCmd("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")))
    assert(Command.parse("fen import") == Right(Command.ImportFenCmd("import")))
    assert(Command.parse("fen importX") == Right(Command.ImportFenCmd("importX")))
    assert(Command.parse("fen export") == Right(Command.ExportFenCmd))
    assert(Command.parse("pgn export") == Right(Command.ExportPgnCmd))
    assert(Command.parse("""pgn import 1. e2e4 *""").isRight)
    assert(Command.parse("parser regex") == Right(Command.SetParserCmd("regex")))
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
    assert(help.message.exists(_.contains("- Remis anbieten: `draw`")))
    assert(help.message.exists(_.contains("- Remis annehmen: `accept`")))
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
    val fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1"
    val res = Controller.update(Controller.initial, s"fen $fen")
    assert(!res.quit)
    assert(res.message.contains("Position set using fastparse."))
    assert(res.game == Fen.parse(fen).toOption.get)
  }

  test("Controller.update ends game when set FEN is already checkmate") {
    val mateFen = "k7/1Q6/2K5/8/8/8/8/8 b - - 0 1"
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

  test("Controller parser selection persists in AppState and affects FEN/PGN workflows") {
    val parserSet = Controller.update(Controller.initialState, "parser regex")
    assert(parserSet.state.parserChoice.id == "regex")
    assert(parserSet.message.contains("Parser set to regex."))

    val show = Controller.update(parserSet.state, "parser")
    assert(show.message.exists(_.contains("Current parser: regex.")))

    val exportedFen = Controller.update(parserSet.state, "fen export")
    assert(exportedFen.message.contains(Fen.encode(Controller.initial)))

    val exportedPgn = Controller.update(
      Controller.update(Controller.initialState, "e2 e4").state,
      "pgn export"
    )
    assert(exportedPgn.message.exists(_.contains("1. e2e4 *")))
    assert(exportedPgn.message.exists(msg => !msg.contains("[FEN ")))

    val pgn = """1. f2f3 e7e5 2. g2g4 d8h4 *"""
    val imported = Controller.update(parserSet.state, s"pgn import $pgn")
    assert(imported.quit)
    assert(imported.message.contains("Checkmate. Black wins."))
    assert(imported.game.isCheckmate)
    assert(imported.state.moveHistory.size == 4)

    val nonMatePgn = """1. e2e4 e7e5 *"""
    val importedNonMate = Controller.update(parserSet.state, s"pgn import $nonMatePgn")
    assert(!importedNonMate.quit)
    assert(importedNonMate.message.contains("PGN imported using regex."))
    assert(importedNonMate.state.moveHistory.size == 2)
  }

  test("Controller.update reports parser selection and PGN import errors without changing state") {
    val initial = Controller.initialState

    val badParser = Controller.update(initial, "parser unknown")
    assert(badParser.state == initial)
    assert(badParser.message.exists(_.contains("Unknown parser")))

    val badPgn = Controller.update(initial, "pgn import 1. e4 *")
    assert(badPgn.state == initial)
    assert(badPgn.message.contains("Invalid PGN movetext."))
  }

  test("Controller.update supports explicit promotion moves") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(4, 6) -> Piece(Color.White, PieceType.Pawn),
        Pos(7, 7) -> Piece(Color.Black, PieceType.King),
        Pos(0, 0) -> Piece(Color.White, PieceType.King)
      )
    )
    val game = Game(board, Color.White)

    val res = Controller.update(game, "e7 e8 n")
    assert(!res.quit)
    assert(res.message.isEmpty)
    assert(res.game.board.pieceAt(Pos(4, 7)).contains(Piece(Color.White, PieceType.Knight)))
    assert(res.game.sideToMove == Color.Black)
  }

  test("Controller.update requires an explicit promotion choice in the TUI") {
    val board = Board.empty.copy(
      pieces = Map(
        Pos(4, 6) -> Piece(Color.White, PieceType.Pawn),
        Pos(7, 7) -> Piece(Color.Black, PieceType.King),
        Pos(0, 0) -> Piece(Color.White, PieceType.King)
      )
    )
    val game = Game(board, Color.White)

    val res = Controller.update(game, "e7 e8")
    assert(!res.quit)
    assert(res.game == game)
    assert(res.message.contains("Promotion required: choose q, r, b, or n."))
  }

  test("Controller supports draw offer and acceptance") {
    val s0 = Controller.initialState
    assert(s0.game.sideToMove == Color.White)

    // White offers draw -> turn flips to Black
    val offer = Controller.update(s0, "draw")
    assert(offer.state.drawOfferedBy.contains(Color.White))
    assert(offer.state.game.sideToMove == Color.Black)
    assert(offer.message.exists(_.contains("White bietet Remis an.")))
    assert(offer.message.exists(_.contains("accept")))
    assert(!offer.quit)

    // Duplicate offer is rejected
    val dupOffer = Controller.update(offer.state, "draw")
    assert(dupOffer.state == offer.state)
    assert(dupOffer.message.contains("Es gibt bereits ein offenes Remis-Angebot."))

    // The offerer (White) cannot accept own offer
    // Simulate by manually setting sideToMove back to White
    val offererTriesAccept = Controller.update(
      offer.state.copy(game = offer.state.game.copy(sideToMove = Color.White)), "accept")
    assert(!offererTriesAccept.quit)
    assert(offererTriesAccept.message.contains("Du kannst dein eigenes Remis-Angebot nicht annehmen."))

    // Moves are blocked while draw offer is pending
    val blockedMove = Controller.update(offer.state, "e7 e5")
    assert(!blockedMove.quit)
    assert(blockedMove.message.exists(_.contains("Remis-Angebot ausstehend.")))
    assert(blockedMove.state == offer.state)

    // Black accepts -> game ends as draw
    val accept = Controller.update(offer.state, "accept")
    assert(accept.quit)
    assert(accept.state.drawAgreed)
    assert(accept.message.contains("Spiel durch Remis-Uebereinkunft beendet.") ||
           accept.message.exists(_.contains("beendet.")))

    // --- Decline path ---
    val offer2 = Controller.update(s0, "draw")
    assert(offer2.state.game.sideToMove == Color.Black)

    val decline = Controller.update(offer2.state, "decline")
    assert(!decline.quit)
    assert(decline.state.drawOfferedBy.isEmpty)
    // Turn flips back to White (the offerer)
    assert(decline.state.game.sideToMove == Color.White)
    assert(decline.message.exists(_.contains("abgelehnt")))
    assert(decline.message.exists(_.contains("White")))

    // After decline, moves are possible again
    val moveAfterDecline = Controller.update(decline.state, "e2 e4")
    assert(!moveAfterDecline.quit)
    assert(moveAfterDecline.state.game.sideToMove == Color.Black)

    // No pending offer -> decline/accept return error
    val noPendingDecline = Controller.update(s0, "decline")
    assert(noPendingDecline.message.contains("Kein Remis-Angebot vorhanden."))
    val noPendingAccept = Controller.update(s0, "accept")
    assert(noPendingAccept.message.contains("Kein Remis-Angebot vorhanden."))
  }

  test("Controller ends game on stalemate during import") {
    val stalematePgn = """[Result "*"]
1. e2e3 a7a5 2. d1h5 a8a6 3. h5a5 h7h5 4. h2h4 a6h6 5. a5c7 f7f6 6. c7d7 e8f7 7. d7b7 d8d3 8. b7b8 d3h7 9. b8c8 f7g6 10. c8e6 *"""
    val res = Controller.update(Controller.initialState, s"pgn import $stalematePgn")
    assert(res.quit)
    assert(res.message.contains("Draw (Stalemate)."))
  }
