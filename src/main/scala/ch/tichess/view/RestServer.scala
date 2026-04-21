package ch.tichess.view

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.*
import ch.tichess.services.{ControllerHttpClient, ServiceConfig}

import scala.concurrent.Await
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object RestServer extends JsonSupport:

  def main(args: Array[String]): Unit =
    implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "TiChessViewService")
    implicit val executionContext: ExecutionContextExecutor = system.executionContext

    val controllerServiceUrl = ServiceConfig.url("CONTROLLER_SERVICE_URL", "http://localhost:8082")
    val controllerClient = new ControllerHttpClient(controllerServiceUrl)
    val port = ServiceConfig.port("VIEW_SERVICE_PORT", 8080)

    val route =
      concat(
        pathEndOrSingleSlash {
          get {
            val html =
              """<!DOCTYPE html>
                |<html lang="en">
                |<head>
                |    <meta charset="UTF-8">
                |    <title>TiChess REST API</title>
                |    <style>
                |        :root {
                |            --bg-color: #0f172a;
                |            --glass-bg: rgba(30, 41, 59, 0.7);
                |            --glass-border: rgba(255, 255, 255, 0.1);
                |            --light-sq: #cbd5e1;
                |            --dark-sq: #475569;
                |            --highlight: rgba(56, 189, 248, 0.5);
                |            --text-color: #f8fafc;
                |        }
                |        body {
                |            background-color: var(--bg-color);
                |            color: var(--text-color);
                |            font-family: 'Inter', -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
                |            margin: 0; display: flex; flex-direction: column; align-items: center;
                |            min-height: 100vh; justify-content: center;
                |        }
                |        .container {
                |            background: var(--glass-bg); backdrop-filter: blur(16px);
                |            border: 1px solid var(--glass-border); border-radius: 1rem;
                |            padding: 2rem; box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.5);
                |            display: flex; flex-direction: column; align-items: center; gap: 1.5rem;
                |        }
                |        h1 {
                |            margin: 0; font-size: 2rem; font-weight: 700; letter-spacing: -0.025em;
                |            background: linear-gradient(to right, #38bdf8, #818cf8);
                |            -webkit-background-clip: text; background-clip: text; color: transparent;
                |        }
                |        .status {
                |            font-size: 1.125rem; font-weight: 500; padding: 0.5rem 1rem;
                |            background: rgba(0,0,0,0.2); border-radius: 0.5rem;
                |        }
                |        .board {
                |            display: grid; grid-template-columns: repeat(8, 4rem); grid-template-rows: repeat(8, 4rem);
                |            border: 4px solid var(--dark-sq); border-radius: 0.25rem;
                |            overflow: hidden; box-shadow: 0 10px 15px -3px rgba(0,0,0,0.4);
                |        }
                |        .square {
                |            width: 4rem; height: 4rem; display: flex; justify-content: center; align-items: center;
                |            font-size: 2.75rem; cursor: pointer; user-select: none;
                |            transition: background-color 0.15s ease, transform 0.1s ease;
                |        }
                |        .square:active { transform: scale(0.95); }
                |        .sq-light { background-color: var(--light-sq); }
                |        .sq-dark { background-color: var(--dark-sq); }
                |        .sq-legal { position: relative; }
                |        .sq-legal::after { content: ''; position: absolute; width: 30%; height: 30%; background: rgba(0,0,0,0.2); border-radius: 50%; top: 35%; left: 35%; pointer-events: none; }
                |        .sq-dark.sq-legal::after { background: rgba(255,255,255,0.2); }
                |        .selected { background-color: var(--highlight) !important; box-shadow: inset 0 0 15px rgba(255,255,255,0.3); }
                |        .modal-overlay {
                |            position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
                |            background: rgba(0,0,0,0.6); backdrop-filter: blur(4px);
                |            display: flex; justify-content: center; align-items: center;
                |            z-index: 50; opacity: 0; pointer-events: none; transition: opacity 0.2s ease;
                |        }
                |        .modal-overlay.active { opacity: 1; pointer-events: all; }
                |        .modal-content {
                |            background: var(--bg-color); border: 1px solid var(--glass-border);
                |            border-radius: 1rem; padding: 2rem; display: flex; gap: 1rem;
                |            box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.7);
                |        }
                |        .promo-btn {
                |            background: var(--glass-bg); border: 1px solid var(--glass-border); border-radius: 0.5rem;
                |            font-size: 2.5rem; width: 4.5rem; height: 4.5rem; cursor: pointer; color: var(--text-color); transition: all 0.2s;
                |        }
                |        .promo-btn:hover { background: var(--highlight); transform: translateY(-2px); }
                |        .piece-white { color: #ffffff; text-shadow: 0 2px 4px rgba(0,0,0,0.6); }
                |        .piece-black { color: #000000; text-shadow: 0 1px 2px rgba(255,255,255,0.6); }
                |        .board.game-over { pointer-events: none; opacity: 0.75; }
                |        .action-bar { display: flex; gap: 0.75rem; align-items: center; flex-wrap: wrap; justify-content: center; }
                |        .action-btn {
                |            padding: 0.45rem 1.1rem; border-radius: 0.5rem; border: 1px solid var(--glass-border);
                |            background: var(--glass-bg); color: var(--text-color); font-size: 0.9rem;
                |            cursor: pointer; transition: all 0.2s;
                |        }
                |        .action-btn:hover:not(:disabled) { background: var(--highlight); }
                |        .action-btn:disabled { opacity: 0.35; cursor: not-allowed; }
                |        .action-btn.accept { background: rgba(34,197,94,0.25); border-color: rgba(34,197,94,0.5); }
                |        .action-btn.accept:hover:not(:disabled) { background: rgba(34,197,94,0.45); }
                |        .action-btn.decline { background: rgba(249,115,22,0.25); border-color: rgba(249,115,22,0.5); }
                |        .action-btn.decline:hover:not(:disabled) { background: rgba(249,115,22,0.45); }
                |        .action-btn.resign { background: rgba(239,68,68,0.25); border-color: rgba(239,68,68,0.5); }
                |        .action-btn.resign:hover:not(:disabled) { background: rgba(239,68,68,0.45); }
                |        .action-btn.new-game { background: rgba(59,130,246,0.25); border-color: rgba(59,130,246,0.5); }
                |        .action-btn.new-game:hover:not(:disabled) { background: rgba(59,130,246,0.45); }
                |        .captured-pieces { font-size: 1.25rem; min-height: 1.5rem; letter-spacing: 2px; color: var(--text-color); margin: 4px 0; }
                |        .main-layout { display: flex; gap: 2rem; justify-content: center; align-items: flex-start; flex-wrap: wrap; max-width: 1200px; margin: 0 auto; }
                |        .sidebar { background: var(--glass-bg); border: 1px solid var(--glass-border); border-radius: 1rem; padding: 1.5rem; display: flex; flex-direction: column; gap: 1rem; min-width: 300px; box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.5); }
                |        .move-list { height: 250px; overflow-y: auto; background: rgba(0,0,0,0.2); border-radius: 0.5rem; padding: 0.5rem; font-family: monospace; font-size: 0.9rem; }
                |        .move-list div { padding: 2px 4px; border-bottom: 1px solid rgba(255,255,255,0.05); }
                |        .notation-box { display: flex; flex-direction: column; gap: 0.5rem; }
                |        .notation-text { width: 100%; height: 80px; background: rgba(0,0,0,0.2); color: var(--text-color); border: 1px solid var(--glass-border); border-radius: 0.5rem; padding: 0.5rem; font-family: monospace; resize: vertical; }
                |        select { background: rgba(0,0,0,0.2); color: var(--text-color); border: 1px solid var(--glass-border); padding: 0.4rem; border-radius: 0.5rem; }
                |    </style>
                |</head>
                |<body>
                |
                |<div class="main-layout">
                |<div class="container">
                |    <h1>TiChess Web GUI</h1>
                |    <div id="status" class="status">Connecting...</div>
                |    <div id="black-captured" class="captured-pieces"></div>
                |    <div id="board" class="board"></div>
                |    <div id="white-captured" class="captured-pieces"></div>
                |    <div class="action-bar">
                |        <button id="btn-draw" class="action-btn" onclick="sendCommand('draw')">Remis anbieten</button>
                |        <button id="btn-accept" class="action-btn accept" style="display:none" onclick="sendCommand('accept')">Remis annehmen ✓</button>
                |        <button id="btn-decline" class="action-btn decline" style="display:none" onclick="sendCommand('decline')">Remis ablehnen ✗</button>
                |        <button id="btn-resign" class="action-btn resign" onclick="sendCommand('resign')">Aufgeben 🏳</button>
                |        <button id="btn-new" class="action-btn new-game" onclick="sendCommand('new')">Neues Spiel 🔄</button>
                |    </div>
                |</div>
                |<div class="sidebar">
                |    <h3>Notation / Parser</h3>
                |    <select id="parser-select" onchange="sendCommand('parser ' + this.value)"></select>
                |    <div id="move-list" class="move-list"></div>
                |    <div class="notation-box">
                |       <textarea id="notation-text" class="notation-text"></textarea>
                |       <div style="display: flex; gap: 0.5rem; flex-wrap: wrap;">
                |         <button class="action-btn" onclick="sendCommand('fen export')">Export FEN</button>
                |         <button class="action-btn" onclick="sendCommand('fen import ' + document.getElementById('notation-text').value)">Import FEN</button>
                |         <button class="action-btn" onclick="sendCommand('pgn export')">Export PGN</button>
                |         <button class="action-btn" onclick="sendCommand('pgn import ' + document.getElementById('notation-text').value)">Import PGN</button>
                |       </div>
                |    </div>
                |</div>
                |</div>
                |
                |<div id="promo-modal" class="modal-overlay">
                |    <div class="modal-content">
                |        <button class="promo-btn" onclick="selectPromotion('q')">&#x265B;&#xFE0E;</button>
                |        <button class="promo-btn" onclick="selectPromotion('r')">&#x265C;&#xFE0E;</button>
                |        <button class="promo-btn" onclick="selectPromotion('b')">&#x265D;&#xFE0E;</button>
                |        <button class="promo-btn" onclick="selectPromotion('n')">&#x265E;&#xFE0E;</button>
                |    </div>
                |</div>
                |
                |<script>
                |    const pieceMap = {
                |        'K': { char: '&#x265A;&#xFE0E;', cls: 'piece-white' }, 'Q': { char: '&#x265B;&#xFE0E;', cls: 'piece-white' },
                |        'R': { char: '&#x265C;&#xFE0E;', cls: 'piece-white' }, 'B': { char: '&#x265D;&#xFE0E;', cls: 'piece-white' },
                |        'N': { char: '&#x265E;&#xFE0E;', cls: 'piece-white' }, 'P': { char: '&#x265F;&#xFE0E;', cls: 'piece-white' },
                |        'k': { char: '&#x265A;&#xFE0E;', cls: 'piece-black' }, 'q': { char: '&#x265B;&#xFE0E;', cls: 'piece-black' },
                |        'r': { char: '&#x265C;&#xFE0E;', cls: 'piece-black' }, 'b': { char: '&#x265D;&#xFE0E;', cls: 'piece-black' },
                |        'n': { char: '&#x265E;&#xFE0E;', cls: 'piece-black' }, 'p': { char: '&#x265F;&#xFE0E;', cls: 'piece-black' }
                |    };
                |    let selectedIdx = null; let currentBoard = new Array(64).fill(null); let pendingPromoMove = null;
                |    let isGameOver = false; let legalMovesData = {};
                |    function algebraic(idx) {
                |        const file = idx % 8; const rank = 7 - Math.floor(idx / 8);
                |        return String.fromCharCode('a'.charCodeAt(0) + file) + (rank + 1);
                |    }
                |    async function fetchGame() {
                |        try {
                |            const response = await fetch('/api/view/game'); const data = await response.json();
                |            document.getElementById('status').innerText = data.statusText;
                |            isGameOver = data.isGameOver;
                |            const boardEl = document.getElementById('board');
                |            if (isGameOver) boardEl.classList.add('game-over'); else boardEl.classList.remove('game-over');
                |            const btnDraw = document.getElementById('btn-draw');
                |            const btnAccept = document.getElementById('btn-accept');
                |            const btnResign = document.getElementById('btn-resign');
                |            btnDraw.disabled = isGameOver || data.drawOffered;
                |            const showDrawResponse = data.drawOffered && !isGameOver;
                |            btnAccept.style.display = showDrawResponse ? 'inline-block' : 'none';
                |            document.getElementById('btn-decline').style.display = showDrawResponse ? 'inline-block' : 'none';
                |            btnResign.disabled = isGameOver;
                |            document.getElementById('white-captured').innerText = data.whiteCaptured || '';
                |            document.getElementById('black-captured').innerText = data.blackCaptured || '';
                |            legalMovesData = data.legalMoves || {};
                |            const moveListEl = document.getElementById('move-list');
                |            moveListEl.innerHTML = '';
                |            (data.moveList || []).forEach(entry => {
                |               const row = document.createElement('div'); row.innerText = entry;
                |               moveListEl.appendChild(row);
                |            });
                |            moveListEl.scrollTop = moveListEl.scrollHeight;
                |            const parserSelect = document.getElementById('parser-select');
                |            if (parserSelect.options.length === 0) {
                |                (data.availableParsers || []).forEach(p => {
                |                    const opt = document.createElement('option'); opt.value = p; opt.innerText = p;
                |                    parserSelect.appendChild(opt);
                |                });
                |            }
                |            parserSelect.value = data.currentParser;
                |            renderFen(data.fen);
                |        } catch (e) { document.getElementById('status').innerText = "Connection lost."; }
                |    }
                |    function renderFen(fen) {
                |        const placement = fen.split(' ')[0]; let idx = 0; currentBoard = new Array(64).fill(null);
                |        for (let i = 0; i < placement.length; i++) {
                |            const c = placement[i];
                |            if (c === '/') continue;
                |            if (c >= '1' && c <= '8') idx += parseInt(c); else { currentBoard[idx] = c; idx++; }
                |        }
                |        drawBoard();
                |    }
                |    function drawBoard() {
                |        const boardEl = document.getElementById('board'); boardEl.innerHTML = '';
                |        for (let i = 0; i < 64; i++) {
                |            const row = Math.floor(i / 8); const col = i % 8; const isLight = (row + col) % 2 === 0;
                |            const sq = document.createElement('div');
                |            sq.className = 'square ' + (isLight ? 'sq-light' : 'sq-dark');
                |            if (selectedIdx === i) sq.classList.add('selected');
                |            if (selectedIdx !== null) {
                |                const alg = algebraic(selectedIdx);
                |                if (legalMovesData[alg] && legalMovesData[alg].includes(algebraic(i))) {
                |                    sq.classList.add('sq-legal');
                |                }
                |            }
                |            const piece = currentBoard[i];
                |            if (piece) sq.innerHTML = `<span class="${pieceMap[piece].cls}">${pieceMap[piece].char}</span>`;
                |            sq.onclick = () => handleSquareClick(i);
                |            boardEl.appendChild(sq);
                |        }
                |    }
                |    function handleSquareClick(idx) {
                |        if (isGameOver) return;
                |        if (selectedIdx === null) {
                |            if (currentBoard[idx]) { selectedIdx = idx; drawBoard(); }
                |        } else {
                |            if (selectedIdx === idx) { selectedIdx = null; drawBoard(); return; }
                |            const fromAlg = algebraic(selectedIdx); const toAlg = algebraic(idx);
                |            const piece = currentBoard[selectedIdx];
                |            if ((piece === 'P' && idx <= 7) || (piece === 'p' && idx >= 56)) {
                |                pendingPromoMove = { from: fromAlg, to: toAlg };
                |                document.getElementById('promo-modal').classList.add('active');
                |            } else { sendMove(fromAlg + " " + toAlg); }
                |            selectedIdx = null; drawBoard();
                |        }
                |    }
                |    function selectPromotion(role) {
                |        document.getElementById('promo-modal').classList.remove('active');
                |        if (pendingPromoMove) { sendMove(pendingPromoMove.from + " " + pendingPromoMove.to + " " + role); pendingPromoMove = null; }
                |    }
                |    async function sendMove(algebraicMove) {
                |        try {
                |            const res = await fetch('/api/controller/update', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ input: algebraicMove }) });
                |            const data = await res.json();
                |            if (!data.success && data.message) document.getElementById('status').innerText = "Illegal: " + data.message;
                |            fetchGame();
                |        } catch (e) { console.error(e); }
                |    }
                |    async function sendCommand(cmd) {
                |        try {
                |            const res = await fetch('/api/controller/update', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ input: cmd }) });
                |            const data = await res.json();
                |            if (data.message) {
                |               if (cmd.includes('export')) {
                |                   document.getElementById('notation-text').value = data.message;
                |                   document.getElementById('status').innerText = "Exported.";
                |               } else {
                |                   document.getElementById('status').innerText = data.message;
                |               }
                |            }
                |            fetchGame();
                |        } catch (e) { console.error(e); }
                |    }
                |    fetchGame();
                |</script>
                |</body>
                |</html>""".stripMargin
            complete(akka.http.scaladsl.model.HttpEntity(
              akka.http.scaladsl.model.ContentTypes.`text/html(UTF-8)`, 
              html
            ))
          }
        },
        pathPrefix("api" / "controller") {
          post {
            path("update") {
              entity(as[CommandRequest]) { req =>
                onComplete(controllerClient.update(req.input)) {
                   case Success(res) => complete(res)
                   case Failure(ex) =>
                      complete(CommandResponse(success = false, Some(ex.getMessage), None, false))
                }
              }
            }
          }
        },
        pathPrefix("api" / "view") {
          get {
            path("game") {
              onComplete(controllerClient.fetchState()) {
                case Success(state) => complete(state)
                case Failure(ex) =>
                  complete(
                    StateResponse(
                      fen = "8/8/8/8/8/8/8/8 w - - 0 1",
                      statusText = s"Controller unavailable: ${ex.getMessage}",
                      isGameOver = true,
                      drawOffered = false,
                      whiteCaptured = "",
                      blackCaptured = "",
                      moveList = Nil,
                      legalMoves = Map.empty,
                      currentParser = "fastparse",
                      availableParsers = List("fastparse")
                    )
                  )
              }
            }
          }
        }
      )

    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"View service online at http://localhost:$port/")
    println("Proxy endpoints ready:")
    println("  POST /api/controller/update")
    println("  GET  /api/view/game")
    Await.result(system.whenTerminated, Duration.Inf)
