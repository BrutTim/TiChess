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
        path("health") {
          get {
            complete("ok")
          }
        },
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
                |        .board-panel { width: 32rem; display: flex; flex-direction: column; gap: 0.6rem; }
                |        .clock-row { display: flex; justify-content: flex-end; }
                |        .clock {
                |            min-width: 7.25rem; padding: 0.55rem 0.9rem; border-radius: 0.85rem; text-align: center;
                |            background: rgba(15, 23, 42, 0.76); border: 1px solid rgba(255,255,255,0.08);
                |            box-shadow: 0 10px 18px -16px rgba(0, 0, 0, 0.85);
                |            display: flex; flex-direction: column; align-items: center; gap: 0.1rem;
                |            transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease, background-color 0.15s ease;
                |        }
                |        .clock-label { font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase; color: #94a3b8; }
                |        .clock-time { font-size: 1.55rem; font-weight: 800; font-variant-numeric: tabular-nums; }
                |        .clock.white-side {
                |            background: rgba(248, 250, 252, 0.94); border-color: rgba(148, 163, 184, 0.3); color: #0f172a;
                |        }
                |        .clock.white-side .clock-label { color: rgba(51, 65, 85, 0.78); }
                |        .clock.active {
                |            background: rgba(30, 41, 59, 0.96); border-color: rgba(96, 165, 250, 0.45);
                |            box-shadow: 0 0 0 1px rgba(96, 165, 250, 0.2), 0 0 20px rgba(59, 130, 246, 0.18);
                |            transform: translateY(-1px);
                |        }
                |        .clock.white-side.active {
                |            background: rgba(255, 255, 255, 0.98); border-color: rgba(59, 130, 246, 0.35);
                |            box-shadow: 0 0 0 1px rgba(96, 165, 250, 0.16), 0 0 18px rgba(148, 163, 184, 0.22);
                |        }
                |        .clock.expired {
                |            background: rgba(127, 29, 29, 0.92); border-color: rgba(248, 113, 113, 0.45);
                |            box-shadow: 0 0 0 1px rgba(248, 113, 113, 0.2), 0 0 20px rgba(239, 68, 68, 0.18);
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
                |        .hint-square { box-shadow: inset 0 0 0 5px rgba(250, 204, 21, 0.88), inset 0 0 24px rgba(250, 204, 21, 0.35); }
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
                |        .board.locked { pointer-events: none; opacity: 0.8; }
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
                |            display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 0.5rem;
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
                |        .sr-only {
                |            position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px;
                |            overflow: hidden; clip: rect(0, 0, 0, 0); white-space: nowrap; border: 0;
                |        }
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
                |        .challenge-meta {
                |            padding: 0.65rem 0.75rem; border-radius: 0.65rem; min-height: 1.2rem;
                |            background: rgba(0,0,0,0.16); border: 1px solid rgba(255,255,255,0.06);
                |            color: #e2e8f0; font-size: 0.95rem; font-weight: 700;
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
                |    <div class="board-panel">
                |        <div class="clock-row">
                |            <div id="clock-black" class="clock">
                |                <span class="clock-label">Schwarz</span>
                |                <span id="clock-black-time" class="clock-time">05:00</span>
                |            </div>
                |        </div>
                |        <div id="black-captured" class="captured-pieces"></div>
                |        <div id="board" class="board"></div>
                |        <div id="white-captured" class="captured-pieces"></div>
                |        <div class="clock-row">
                |            <div id="clock-white" class="clock white-side">
                |                <span class="clock-label">Weiß</span>
                |                <span id="clock-white-time" class="clock-time">05:00</span>
                |            </div>
                |        </div>
                |    </div>
                |</div>
                |<div class="sidebar">
                |    <div class="tab-bar" role="tablist" aria-label="Schach Seitenbereich">
                |        <button id="tab-btn-game" class="tab-btn active" type="button" role="tab" aria-selected="true" aria-controls="tab-game" onclick="switchTab('game')">
                |            <span class="tab-icon" aria-hidden="true">&#x265E;&#xFE0E;</span>
                |            <span class="tab-label sr-only">Spiel</span>
                |        </button>
                |        <button id="tab-btn-io" class="tab-btn" type="button" role="tab" aria-selected="false" aria-controls="tab-io" onclick="switchTab('io')">
                |            <span class="tab-icon" aria-hidden="true">📄</span>
                |            <span class="tab-label sr-only">Import / Export</span>
                |        </button>
                |        <button id="tab-btn-challenges" class="tab-btn" type="button" role="tab" aria-selected="false" aria-controls="tab-challenges" onclick="switchTab('challenges')">
                |            <span class="tab-icon" aria-hidden="true">🧩</span>
                |            <span class="tab-label sr-only">Challenges</span>
                |        </button>
                |    </div>
                |    <section id="tab-game" class="tab-panel active" role="tabpanel" aria-labelledby="tab-btn-game">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">⚡</span><span>Start</span></h3>
                |            <div class="future-controls">
                |                <div class="field-group">
                |                    <label for="time-mode">Zeitmodus</label>
                |                    <select id="time-mode">
                |                        <option value="60000">1 Minute</option>
                |                        <option value="180000">3 Minuten</option>
                |                        <option value="300000" selected>5 Minuten</option>
                |                        <option value="600000">10 Minuten</option>
                |                    </select>
                |                </div>
                |                <button id="btn-start-game" class="action-btn start-btn" type="button" onclick="startTimedGame()">Partie starten</button>
                |                <div class="action-grid">
                |                    <button class="action-btn" type="button" onclick="sendCommand('bot off')">Bot aus</button>
                |                    <button class="action-btn" type="button" onclick="sendCommand('bot black')">Bot Schwarz</button>
                |                    <button class="action-btn" type="button" onclick="sendCommand('bot white')">Bot Weiß</button>
                |                </div>
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
                |    <section id="tab-challenges" class="tab-panel" role="tabpanel" aria-labelledby="tab-btn-challenges">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span>Training</span></h3>
                |            <div class="future-controls">
                |                <div id="challenge-side" class="challenge-meta">Bereit.</div>
                |                <button id="btn-random-challenge" class="action-btn start-btn" type="button" onclick="startRandomChallenge()">Random Challenge</button>
                |                <button id="btn-challenge-hint" class="action-btn" type="button" onclick="showChallengeHint()" disabled>Hinweis</button>
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
                |    let challengeActive = false;
                |    let challengeHintFrom = null;
                |    let challengeHintVisible = false;
                |    let challengeSideToMove = null;
                |    let transientStatusText = null;
                |    let serverStatusText = 'Connecting...';
                |    let currentSideToMove = 'w';
                |    let activeBot = null;
                |    let botInFlight = false;
                |    let lastBotMoveForFen = null;
                |    let gameStarted = false;
                |    let gameTimedOut = false;
                |    let timeoutLoser = null;
                |    let clockBaseMs = 300000;
                |    let whiteRemainingMs = clockBaseMs;
                |    let blackRemainingMs = clockBaseMs;
                |    let activeClock = null;
                |    let timerHandle = null;
                |    let lastTickAt = 0;
                |    function algebraic(idx) {
                |        const file = idx % 8; const rank = 7 - Math.floor(idx / 8);
                |        return String.fromCharCode('a'.charCodeAt(0) + file) + (rank + 1);
                |    }
                |    function selectedModeMs() {
                |        return parseInt(document.getElementById('time-mode').value, 10);
                |    }
                |    function pad(value) {
                |        return String(value).padStart(2, '0');
                |    }
                |    function formatClock(ms) {
                |        const totalSeconds = Math.max(0, Math.ceil(ms / 1000));
                |        const minutes = Math.floor(totalSeconds / 60);
                |        const seconds = totalSeconds % 60;
                |        return `${pad(minutes)}:${pad(seconds)}`;
                |    }
                |    function updateClockDisplay() {
                |        document.getElementById('clock-white-time').innerText = formatClock(whiteRemainingMs);
                |        document.getElementById('clock-black-time').innerText = formatClock(blackRemainingMs);
                |        const whiteClock = document.getElementById('clock-white');
                |        const blackClock = document.getElementById('clock-black');
                |        whiteClock.classList.toggle('active', gameStarted && !gameTimedOut && activeClock === 'w' && !isGameOver);
                |        blackClock.classList.toggle('active', gameStarted && !gameTimedOut && activeClock === 'b' && !isGameOver);
                |        whiteClock.classList.toggle('expired', gameTimedOut && timeoutLoser === 'w');
                |        blackClock.classList.toggle('expired', gameTimedOut && timeoutLoser === 'b');
                |    }
                |    function stopClockTicker() {
                |        if (timerHandle !== null) {
                |            clearInterval(timerHandle);
                |            timerHandle = null;
                |        }
                |    }
                |    function syncActiveClockFromState() {
                |        if (!gameStarted || isGameOver || gameTimedOut) {
                |            activeClock = null;
                |        } else {
                |            activeClock = currentSideToMove;
                |        }
                |        lastTickAt = Date.now();
                |        updateClockDisplay();
                |    }
                |    function tickClocks() {
                |        if (!gameStarted || isGameOver || gameTimedOut || !activeClock) return;
                |        const now = Date.now();
                |        const elapsed = now - lastTickAt;
                |        lastTickAt = now;
                |        if (activeClock === 'w') whiteRemainingMs = Math.max(0, whiteRemainingMs - elapsed);
                |        if (activeClock === 'b') blackRemainingMs = Math.max(0, blackRemainingMs - elapsed);
                |        if (whiteRemainingMs === 0 || blackRemainingMs === 0) {
                |            gameTimedOut = true;
                |            timeoutLoser = whiteRemainingMs === 0 ? 'w' : 'b';
                |            activeClock = null;
                |            stopClockTicker();
                |        }
                |        updateClockDisplay();
                |        updateStatusText();
                |        updateInteractionState();
                |    }
                |    function startClockTicker() {
                |        stopClockTicker();
                |        lastTickAt = Date.now();
                |        timerHandle = setInterval(tickClocks, 200);
                |    }
                |    function resetClocksToSelection() {
                |        clockBaseMs = selectedModeMs();
                |        whiteRemainingMs = clockBaseMs;
                |        blackRemainingMs = clockBaseMs;
                |        gameStarted = false;
                |        gameTimedOut = false;
                |        timeoutLoser = null;
                |        activeClock = null;
                |        stopClockTicker();
                |        updateClockDisplay();
                |        updateStatusText();
                |        updateInteractionState();
                |    }
                |    async function startTimedGame() {
                |        if (challengeActive) return;
                |        clockBaseMs = selectedModeMs();
                |        whiteRemainingMs = clockBaseMs;
                |        blackRemainingMs = clockBaseMs;
                |        gameTimedOut = false;
                |        timeoutLoser = null;
                |        selectedIdx = null;
                |        challengeHintVisible = false;
                |        gameStarted = true;
                |        activeClock = 'w';
                |        lastTickAt = Date.now();
                |        updateClockDisplay();
                |        updateStatusText();
                |        updateInteractionState();
                |        startClockTicker();
                |        fetchGame();
                |    }
                |    function timedOutMessage() {
                |        if (timeoutLoser === 'w') return 'Weiß verliert auf Zeit. Schwarz gewinnt.';
                |        if (timeoutLoser === 'b') return 'Schwarz verliert auf Zeit. Weiß gewinnt.';
                |        return 'Zeit abgelaufen.';
                |    }
                |    function updateStatusText() {
                |        const statusEl = document.getElementById('status');
                |        if (gameTimedOut) {
                |            statusEl.innerText = timedOutMessage();
                |            return;
                |        }
                |        if (challengeActive) {
                |            statusEl.innerText = serverStatusText;
                |            return;
                |        }
                |        if (!gameStarted && !isGameOver) {
                |            statusEl.innerText = `Bereit zum Starten • ${serverStatusText}`;
                |            return;
                |        }
                |        statusEl.innerText = serverStatusText;
                |    }
                |    function updateInteractionState() {
                |        const boardEl = document.getElementById('board');
                |        const locallyBlocked = (!gameStarted && !challengeActive) || gameTimedOut || (activeBot && activeBot === currentSideToMove);
                |        boardEl.classList.toggle('game-over', isGameOver || gameTimedOut);
                |        boardEl.classList.toggle('locked', locallyBlocked && !isGameOver && !gameTimedOut);
                |        const startBtn = document.getElementById('btn-start-game');
                |        const drawBtn = document.getElementById('btn-draw');
                |        const resignBtn = document.getElementById('btn-resign');
                |        const newBtn = document.getElementById('btn-new');
                |        const acceptBtn = document.getElementById('btn-accept');
                |        const declineBtn = document.getElementById('btn-decline');
                |        const hintBtn = document.getElementById('btn-challenge-hint');
                |        startBtn.disabled = gameStarted || challengeActive || isGameOver || gameTimedOut;
                |        document.getElementById('time-mode').disabled = gameStarted || challengeActive;
                |        drawBtn.disabled = !gameStarted || challengeActive || isGameOver || gameTimedOut;
                |        resignBtn.disabled = !gameStarted || challengeActive || isGameOver || gameTimedOut;
                |        newBtn.disabled = false;
                |        acceptBtn.disabled = !gameStarted || challengeActive || isGameOver || gameTimedOut;
                |        declineBtn.disabled = !gameStarted || challengeActive || isGameOver || gameTimedOut;
                |        hintBtn.disabled = !challengeActive || isGameOver || !challengeHintFrom;
                |    }
                |    function switchTab(tabName) {
                |        activeTab = tabName;
                |        ['game', 'io', 'challenges'].forEach(tab => {
                |            const active = tabName === tab;
                |            document.getElementById('tab-btn-' + tab).classList.toggle('active', active);
                |            document.getElementById('tab-btn-' + tab).setAttribute('aria-selected', String(active));
                |            document.getElementById('tab-' + tab).classList.toggle('active', active);
                |        });
                |    }
                |    function maybeAutoBotMove(fen) {
                |        try {
                |            if (!activeBot) return;
                |            if (!gameStarted) return;
                |            if (challengeActive) return;
                |            if (isGameOver || gameTimedOut) return;
                |            if (botInFlight) return;
                |            if (activeBot !== currentSideToMove) return;
                |            if (lastBotMoveForFen === fen) return;
                |
                |            botInFlight = true;
                |            lastBotMoveForFen = fen;
                |            sendCommand('bot move');
                |        } catch (e) {
                |            console.error(e);
                |            botInFlight = false;
                |        }
                |    }
                |    async function fetchGame() {
                |        try {
                |            const response = await fetch('/api/view/game'); const data = await response.json();
                |            isGameOver = data.isGameOver;
                |            challengeActive = (data.statusText || '').startsWith('Challenge aktiv');
                |            challengeHintFrom = data.challengeHintFrom || null;
                |            challengeSideToMove = data.challengeSideToMove || null;
                |            activeBot = data.activeBot || null;
                |            serverStatusText = transientStatusText || data.statusText;
                |            transientStatusText = null;
                |            currentSideToMove = (data.fen.split(' ')[1] || 'w').trim();
                |            botInFlight = false; // allow triggering again for the new position
                |            if (isGameOver) {
                |                gameStarted = false;
                |                activeClock = null;
                |                stopClockTicker();
                |            } else if (gameStarted && !gameTimedOut) {
                |                syncActiveClockFromState();
                |            }
                |            const btnDraw = document.getElementById('btn-draw');
                |            const btnAccept = document.getElementById('btn-accept');
                |            const btnResign = document.getElementById('btn-resign');
                |            btnDraw.disabled = !gameStarted || challengeActive || isGameOver || gameTimedOut || data.drawOffered;
                |            const showDrawResponse = data.drawOffered && !isGameOver;
                |            btnAccept.style.display = showDrawResponse ? 'inline-block' : 'none';
                |            document.getElementById('btn-decline').style.display = showDrawResponse ? 'inline-block' : 'none';
                |            btnResign.disabled = !gameStarted || challengeActive || isGameOver || gameTimedOut;
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
                |            maybeAutoBotMove(data.fen);
                |            updateChallengePanel();
                |            updateClockDisplay();
                |            updateStatusText();
                |            updateInteractionState();
                |            switchTab(activeTab);
                |        } catch (e) {
                |            serverStatusText = 'Connection lost.';
                |            updateStatusText();
                |        }
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
                |            if (challengeHintVisible && challengeHintFrom === algebraic(i)) sq.classList.add('hint-square');
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
                |        if (isGameOver || (!gameStarted && !challengeActive) || gameTimedOut) return;
                |        challengeHintVisible = false;
                |        const clickedPiece = currentBoard[idx];
                |        if (selectedIdx === null) {
                |            if (clickedPiece) { selectedIdx = idx; drawBoard(); }
                |        } else {
                |            if (selectedIdx === idx) { selectedIdx = null; drawBoard(); return; }
                |            const fromAlg = algebraic(selectedIdx); const toAlg = algebraic(idx);
                |            const piece = currentBoard[selectedIdx];
                |            const isLegalDest = legalMovesData[fromAlg] && legalMovesData[fromAlg].includes(toAlg);
                |            if (!isLegalDest && clickedPiece && legalMovesData[toAlg]) {
                |                selectedIdx = idx;
                |                drawBoard();
                |                return;
                |            }
                |            if (isLegalDest && ((piece === 'P' && idx <= 7) || (piece === 'p' && idx >= 56))) {
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
                |        if ((!gameStarted && !challengeActive) || isGameOver || gameTimedOut) return;
                |        try {
                |            const res = await fetch('/api/controller/update', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ input: algebraicMove }) });
                |            const data = await res.json();
                |            if (!data.success && data.message) serverStatusText = "Illegal: " + data.message;
                |            if (data.success && data.message) transientStatusText = data.message;
                |            challengeHintVisible = false;
                |            fetchGame();
                |        } catch (e) { console.error(e); }
                |    }
                |    async function sendCommand(cmd, options = {}) {
                |        try {
                |            const res = await fetch('/api/controller/update', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ input: cmd }) });
                |            const data = await res.json();
                |            if (data.message) {
                |               if (cmd.includes('export')) {
                |                   document.getElementById('notation-text').value = data.message;
                |                   serverStatusText = "Exported.";
                |               } else {
                |                   transientStatusText = data.message;
                |                   serverStatusText = data.message;
                |               }
                |            }
                |            if (cmd.startsWith('challenge ')) {
                |               challengeActive = true;
                |               challengeHintVisible = false;
                |               isGameOver = false;
                |               gameStarted = false;
                |               gameTimedOut = false;
                |               activeClock = null;
                |               stopClockTicker();
                |            }
                |            if (cmd === 'new' || cmd.startsWith('fen import') || cmd.startsWith('pgn import')) {
                |               challengeActive = false;
                |               resetClocksToSelection();
                |            }
                |            if (!options.skipFetch) fetchGame();
                |        } catch (e) { console.error(e); }
                |    }
                |    function updateChallengePanel() {
                |        const sideEl = document.getElementById('challenge-side');
                |        if (challengeActive && challengeSideToMove) {
                |            sideEl.innerText = challengeSideToMove + ' zieht.';
                |        } else if (serverStatusText === 'Challenge geloest.' || serverStatusText === 'Challenge geloest!') {
                |            sideEl.innerText = 'Geloest.';
                |        } else {
                |            sideEl.innerText = 'Bereit.';
                |        }
                |    }
                |    function showChallengeHint() {
                |        if (!challengeActive || isGameOver || !challengeHintFrom) return;
                |        challengeHintVisible = true;
                |        drawBoard();
                |    }
                |    function startRandomChallenge() {
                |        sendCommand('challenge random');
                |    }
                |    document.getElementById('time-mode').addEventListener('change', () => {
                |        if (!gameStarted) resetClocksToSelection();
                |    });
                |    resetClocksToSelection();
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
            concat(
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
                      availableParsers = List("fastparse"),
                      challengeHintFrom = None,
                      challengeSideToMove = None,
                      activeBot = None
                    )
                    )
                }
              }
            )
          }
        }
      )

    Http().newServerAt("0.0.0.0", port).bind(route)
    println(s"View service online at http://localhost:$port/")
    println("Proxy endpoints ready:")
    println("  POST /api/controller/update")
    println("  GET  /api/view/game")
    Await.result(system.whenTerminated, Duration.Inf)
