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
                |            align-content: start;
                |            line-height: 0;
                |        }
                |        .square {
                |            width: 4rem; height: 4rem; display: flex; justify-content: center; align-items: center;
                |            font-size: 2.75rem; cursor: pointer; user-select: none;
                |            transition: background-color 0.15s ease, transform 0.1s ease;
                |            position: relative;
                |            line-height: 1;
                |        }
                |        .square:active { transform: scale(0.95); }
                |        .sq-light { background-color: var(--light-sq); }
                |        .sq-dark { background-color: var(--dark-sq); }
                |        .sq-legal { position: relative; }
                |        .sq-legal::after { content: ''; position: absolute; width: 30%; height: 30%; background: rgba(0,0,0,0.2); border-radius: 50%; top: 35%; left: 35%; pointer-events: none; }
                |        .sq-dark.sq-legal::after { background: rgba(255,255,255,0.2); }
                |        .selected { background-color: var(--highlight) !important; box-shadow: inset 0 0 15px rgba(255,255,255,0.3); }
                |        .last-move { box-shadow: inset 0 0 0 4px rgba(134, 239, 172, 0.45); }
                |        .sq-light.last-move { background-color: #c8ebc7; }
                |        .sq-dark.last-move { background-color: #7baa72; }
                |        .coord-label {
                |            position: absolute; font-size: 0.78rem; font-weight: 700; line-height: 1;
                |            opacity: 0.92; user-select: none; pointer-events: none;
                |        }
                |        .coord-rank { top: 0.28rem; left: 0.35rem; }
                |        .coord-file { right: 0.35rem; bottom: 0.28rem; }
                |        .sq-light .coord-label { color: rgba(71, 85, 105, 0.9); }
                |        .sq-dark .coord-label { color: rgba(226, 232, 240, 0.92); }
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
                |        .sidebar {
                |            background: var(--glass-bg); border: 1px solid var(--glass-border); border-radius: 1rem; padding: 1.5rem;
                |            display: flex; flex-direction: column; gap: 1rem; min-width: 340px; width: 380px;
                |            box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.5);
                |        }
                |        .tab-bar {
                |            display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.5rem;
                |            background: rgba(15, 23, 42, 0.55); border: 1px solid rgba(255,255,255,0.06);
                |            border-radius: 0.9rem; padding: 0.35rem;
                |        }
                |        .tab-btn {
                |            border: 0; border-radius: 0.7rem; padding: 0.85rem 0.95rem; text-align: center;
                |            background: transparent; color: #cbd5e1; cursor: pointer; transition: all 0.2s ease;
                |            display: flex; flex-direction: column; align-items: center; gap: 0.2rem;
                |        }
                |        .tab-btn:hover { background: rgba(255,255,255,0.06); color: #f8fafc; }
                |        .tab-btn.active {
                |            background: linear-gradient(135deg, rgba(56, 189, 248, 0.22), rgba(129, 140, 248, 0.18));
                |            color: #f8fafc; box-shadow: inset 0 0 0 1px rgba(125, 211, 252, 0.18);
                |        }
                |        .tab-icon { font-size: 1.2rem; line-height: 1; }
                |        .tab-label { font-size: 0.98rem; font-weight: 700; }
                |        .tab-panel { display: none; flex-direction: column; gap: 1rem; }
                |        .tab-panel.active { display: flex; }
                |        .panel-card {
                |            background: rgba(15, 23, 42, 0.5); border: 1px solid rgba(255,255,255,0.06);
                |            border-radius: 0.9rem; padding: 1rem; display: flex; flex-direction: column; gap: 0.8rem;
                |        }
                |        .panel-card h3 { margin: 0; font-size: 1.05rem; }
                |        .mini-title { display: flex; align-items: center; gap: 0.5rem; margin: 0; font-size: 1rem; }
                |        .mini-icon { font-size: 1.05rem; }
                |        .move-list {
                |            height: 280px; overflow-y: auto; background: rgba(0,0,0,0.18); border-radius: 0.75rem;
                |            padding: 0.6rem; font-family: monospace; font-size: 0.95rem; border: 1px solid rgba(255,255,255,0.05);
                |        }
                |        .move-list div { padding: 0.45rem 0.55rem; border-radius: 0.45rem; border-bottom: 1px solid rgba(255,255,255,0.05); }
                |        .move-list div:last-child { border-bottom: 0; }
                |        .move-list .latest { background: rgba(134, 239, 172, 0.14); border: 1px solid rgba(134, 239, 172, 0.18); }
                |        .action-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.75rem; }
                |        .action-grid .action-btn.wide { grid-column: 1 / -1; }
                |        .notation-box { display: flex; flex-direction: column; gap: 0.75rem; }
                |        .field-group { display: flex; flex-direction: column; gap: 0.45rem; }
                |        .field-group label { font-size: 0.85rem; font-weight: 700; color: #e2e8f0; }
                |        .hint { font-size: 0.8rem; color: #94a3b8; }
                |        .notation-text {
                |            width: 100%; min-height: 90px; background: rgba(0,0,0,0.2); color: var(--text-color);
                |            border: 1px solid var(--glass-border); border-radius: 0.75rem; padding: 0.75rem;
                |            font-family: monospace; resize: vertical; box-sizing: border-box;
                |        }
                |        select {
                |            background: rgba(0,0,0,0.2); color: var(--text-color); border: 1px solid var(--glass-border);
                |            padding: 0.7rem 0.75rem; border-radius: 0.75rem;
                |        }
                |        .future-controls { display: grid; gap: 0.75rem; }
                |        .future-controls .action-btn { width: 100%; }
                |        .action-btn:disabled.future-disabled {
                |            opacity: 0.5; cursor: not-allowed; border-style: dashed;
                |        }
                |        .start-btn {
                |            width: 100%; padding: 0.95rem 1.2rem; font-size: 1.15rem; font-weight: 700;
                |            background: linear-gradient(180deg, rgba(132, 204, 22, 0.95), rgba(101, 163, 13, 0.95));
                |            border-color: rgba(163, 230, 53, 0.45); color: #f8fafc;
                |        }
                |        .start-btn:disabled { opacity: 0.55; }
                |        @media (max-width: 980px) {
                |            .sidebar { width: min(100%, 42rem); }
                |        }
                |        @media (max-width: 640px) {
                |            .sidebar { min-width: 0; width: 100%; }
                |            .action-grid, .tab-bar { grid-template-columns: 1fr; }
                |        }
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
                |</div>
                |<div class="sidebar">
                |    <div class="tab-bar" role="tablist" aria-label="Schach Seitenbereich">
                |        <button id="tab-btn-game" class="tab-btn active" type="button" role="tab" aria-selected="true" aria-controls="tab-game" onclick="switchTab('game')">
                |            <span class="tab-icon">🎮</span>
                |            <span class="tab-label">Spiel</span>
                |        </button>
                |        <button id="tab-btn-io" class="tab-btn" type="button" role="tab" aria-selected="false" aria-controls="tab-io" onclick="switchTab('io')">
                |            <span class="tab-icon">📄</span>
                |            <span class="tab-label">Import / Export</span>
                |        </button>
                |    </div>
                |    <section id="tab-game" class="tab-panel active" role="tabpanel" aria-labelledby="tab-btn-game">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">⚡</span><span>Start</span></h3>
                |            <div class="future-controls">
                |                <div class="field-group">
                |                    <label for="time-mode">Zeitmodus</label>
                |                    <select id="time-mode" disabled>
                |                        <option>Ohne Zeit</option>
                |                        <option>1 Minute</option>
                |                        <option>3 Minuten</option>
                |                        <option>5 Minuten</option>
                |                        <option>10 Minuten</option>
                |                    </select>
                |                </div>
                |                <button class="action-btn start-btn future-disabled" type="button" disabled>Partie starten</button>
                |            </div>
                |        </div>
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">🕹️</span><span>Aktionen</span></h3>
                |            <div class="action-grid">
                |                <button id="btn-draw" class="action-btn" onclick="sendCommand('draw')">Remis anbieten</button>
                |                <button id="btn-resign" class="action-btn resign" onclick="sendCommand('resign')">Aufgeben</button>
                |                <button id="btn-accept" class="action-btn accept wide" style="display:none" onclick="sendCommand('accept')">Remis annehmen</button>
                |                <button id="btn-decline" class="action-btn decline wide" style="display:none" onclick="sendCommand('decline')">Remis ablehnen</button>
                |                <button id="btn-new" class="action-btn new-game wide" onclick="sendCommand('new')">Neues Spiel</button>
                |            </div>
                |        </div>
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">📜</span><span>Zughistorie</span></h3>
                |            <div id="move-list" class="move-list"></div>
                |        </div>
                |    </section>
                |    <section id="tab-io" class="tab-panel" role="tabpanel" aria-labelledby="tab-btn-io">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">🧩</span><span>Notation</span></h3>
                |            <div class="notation-box">
                |                <div class="field-group">
                |                    <label for="parser-select">Parser / Notation</label>
                |                    <select id="parser-select" onchange="sendCommand('parser ' + this.value)"></select>
                |                </div>
                |                <div class="field-group">
                |                    <label for="notation-text">FEN / PGN Eingabe</label>
                |                    <textarea id="notation-text" class="notation-text" placeholder="FEN oder PGN hier einfuegen..."></textarea>
                |                </div>
                |                <div class="action-grid">
                |                    <button class="action-btn" onclick="sendCommand('fen export')">Export FEN</button>
                |                    <button class="action-btn" onclick="sendCommand('fen import ' + document.getElementById('notation-text').value)">Import FEN</button>
                |                    <button class="action-btn" onclick="sendCommand('pgn export')">Export PGN</button>
                |                    <button class="action-btn" onclick="sendCommand('pgn import ' + document.getElementById('notation-text').value)">Import PGN</button>
                |                </div>
                |            </div>
                |        </div>
                |    </section>
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
                |    let isGameOver = false; let legalMovesData = {}; let lastMoveSquares = [];
                |    let activeTab = 'game';
                |    function algebraic(idx) {
                |        const file = idx % 8; const rank = 7 - Math.floor(idx / 8);
                |        return String.fromCharCode('a'.charCodeAt(0) + file) + (rank + 1);
                |    }
                |    function switchTab(tabName) {
                |        activeTab = tabName;
                |        const isGame = tabName === 'game';
                |        document.getElementById('tab-btn-game').classList.toggle('active', isGame);
                |        document.getElementById('tab-btn-io').classList.toggle('active', !isGame);
                |        document.getElementById('tab-btn-game').setAttribute('aria-selected', String(isGame));
                |        document.getElementById('tab-btn-io').setAttribute('aria-selected', String(!isGame));
                |        document.getElementById('tab-game').classList.toggle('active', isGame);
                |        document.getElementById('tab-io').classList.toggle('active', !isGame);
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
                |            document.getElementById('btn-new').disabled = false;
                |            document.getElementById('white-captured').innerText = data.whiteCaptured || '';
                |            document.getElementById('black-captured').innerText = data.blackCaptured || '';
                |            legalMovesData = data.legalMoves || {};
                |            lastMoveSquares = [data.lastMoveFrom, data.lastMoveTo].filter(Boolean);
                |            const moveListEl = document.getElementById('move-list');
                |            moveListEl.innerHTML = '';
                |            (data.moveList || []).forEach((entry, index, allEntries) => {
                |               const row = document.createElement('div'); row.innerText = entry;
                |               if (index === allEntries.length - 1) row.classList.add('latest');
                |               moveListEl.appendChild(row);
                |            });
                |            if ((data.moveList || []).length === 0) {
                |               const row = document.createElement('div');
                |               row.innerText = 'Noch keine Zuege gespielt.';
                |               moveListEl.appendChild(row);
                |            }
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
                |            switchTab(activeTab);
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
                |            if (lastMoveSquares.includes(algebraic(i))) sq.classList.add('last-move');
                |            if (selectedIdx !== null) {
                |                const alg = algebraic(selectedIdx);
                |                if (legalMovesData[alg] && legalMovesData[alg].includes(algebraic(i))) {
                |                    sq.classList.add('sq-legal');
                |                }
                |            }
                |            const piece = currentBoard[i];
                |            if (piece) sq.innerHTML = `<span class="${pieceMap[piece].cls}">${pieceMap[piece].char}</span>`;
                |            if (col === 0) sq.appendChild(createSquareCoord(String(8 - row), 'coord-rank'));
                |            if (row === 7) sq.appendChild(createSquareCoord(String.fromCharCode('a'.charCodeAt(0) + col), 'coord-file'));
                |            sq.onclick = () => handleSquareClick(i);
                |            boardEl.appendChild(sq);
                |        }
                |    }
                |    function createSquareCoord(label, className) {
                |        const marker = document.createElement('span');
                |        marker.className = 'coord-label ' + className;
                |        marker.innerText = label;
                |        return marker;
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
                      lastMoveFrom = None,
                      lastMoveTo = None,
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
