package ch.tichess.view

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.*
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken, RawHeader}
import akka.http.scaladsl.server.Directives.*
import akka.util.ByteString
import ch.tichess.services.{ControllerHttpClient, ServiceConfig}

import scala.concurrent.Await
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration.*
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
                |    <title>TiChess</title>
                |    <style>
                |        :root {
                |            --page-bg: #171612;
                |            --surface: #24231d;
                |            --surface-2: #302e25;
                |            --surface-3: #3b382d;
                |            --border: rgba(232, 218, 190, 0.16);
                |            --light-sq: #e2cfa6;
                |            --dark-sq: #6f7f4f;
                |            --highlight: rgba(235, 177, 77, 0.48);
                |            --text-color: #fbf4e6;
                |            --muted: #b8ad96;
                |            --accent: #d69b3b;
                |            --danger: #c95f48;
                |            --success: #7c9f52;
                |        }
                |        * { box-sizing: border-box; }
                |        html { min-height: 100%; background: var(--page-bg); }
                |        body {
                |            background:
                |                radial-gradient(circle at 12% 8%, rgba(214, 155, 59, 0.12), transparent 28rem),
                |                linear-gradient(135deg, #171612 0%, #24231d 48%, #1b2418 100%);
                |            color: var(--text-color);
                |            font-family: 'Inter', -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
                |            margin: 0; min-height: 100vh;
                |        }
                |        .app-shell {
                |            min-height: 100vh; display: flex; flex-direction: column;
                |        }
                |        .topbar {
                |            min-height: 4.5rem; display: flex; align-items: center; justify-content: space-between;
                |            gap: 1.5rem; padding: 1rem clamp(1rem, 3vw, 2.75rem);
                |            background: rgba(23, 22, 18, 0.86); border-bottom: 1px solid var(--border);
                |            backdrop-filter: blur(14px); position: sticky; top: 0; z-index: 20;
                |        }
                |        .brand {
                |            display: flex; flex-direction: column; gap: 0.12rem; min-width: 10rem;
                |        }
                |        h1 {
                |            margin: 0; font-size: clamp(1.5rem, 2.2vw, 2.35rem); font-weight: 800; letter-spacing: 0;
                |        }
                |        .brand-subtitle {
                |            color: var(--muted); font-size: 0.85rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase;
                |        }
                |        .status {
                |            width: min(52rem, 100%); font-size: 1rem; font-weight: 700; padding: 0.78rem 1rem;
                |            background: rgba(48, 46, 37, 0.74); border: 1px solid var(--border); border-radius: 0.5rem;
                |            color: #f8ead0; box-shadow: inset 0 1px 0 rgba(255,255,255,0.05);
                |        }
                |        .main-layout {
                |            flex: 1; display: grid; grid-template-columns: 13.5rem minmax(28rem, 1fr) minmax(22rem, 27rem);
                |            gap: clamp(1rem, 2.6vw, 2.1rem); align-items: start;
                |            width: 100%; margin: 0; padding: 0 clamp(1rem, 2.4vw, 2.35rem) clamp(1rem, 2.4vw, 2.35rem) 0;
                |        }
                |        .main-layout.panel-only {
                |            grid-template-columns: 13.5rem minmax(0, 1fr);
                |        }
                |        .main-layout.panel-only .board-area { display: none; }
                |        .main-layout.panel-only .sidebar {
                |            grid-column: 2; max-width: min(88rem, 100%); min-height: auto; position: static;
                |        }
                |        .board-area {
                |            min-height: calc(100vh - 7rem); display: grid; place-items: start center;
                |            padding: clamp(0.5rem, 1.8vw, 1.5rem);
                |        }
                |        .board-panel {
                |            width: min(100%, calc(100vh - 17rem), 46rem); min-width: min(100%, 28rem);
                |            display: flex; flex-direction: column; gap: 0.35rem; position: relative;
                |        }
                |        .clock-row {
                |            position: absolute; right: 0.85rem; z-index: 4;
                |            display: flex; justify-content: flex-end; min-height: 0; pointer-events: none;
                |        }
                |        .clock-row:first-child { top: 0.85rem; }
                |        .clock-row:last-child { bottom: 0.85rem; }
                |        .clock {
                |            min-width: 7.25rem; padding: 0.55rem 0.9rem; border-radius: 0.85rem; text-align: center;
                |            background: rgba(36, 35, 29, 0.88); border: 1px solid var(--border);
                |            box-shadow: 0 10px 18px -16px rgba(0, 0, 0, 0.85);
                |            display: flex; flex-direction: column; align-items: center; gap: 0.1rem;
                |            transition: transform 0.15s ease, box-shadow 0.15s ease, border-color 0.15s ease, background-color 0.15s ease;
                |        }
                |        .clock-label { font-size: 0.72rem; letter-spacing: 0.08em; text-transform: uppercase; color: var(--muted); }
                |        .clock-time { font-size: 1.55rem; font-weight: 800; font-variant-numeric: tabular-nums; }
                |        .clock.white-side {
                |            background: rgba(251, 244, 230, 0.96); border-color: rgba(214, 155, 59, 0.3); color: #171612;
                |        }
                |        .clock.white-side .clock-label { color: rgba(75, 67, 50, 0.78); }
                |        .clock.active {
                |            background: rgba(48, 46, 37, 0.98); border-color: rgba(214, 155, 59, 0.5);
                |            box-shadow: 0 0 0 1px rgba(214, 155, 59, 0.18), 0 0 20px rgba(214, 155, 59, 0.16);
                |            transform: translateY(-1px);
                |        }
                |        .clock.white-side.active {
                |            background: rgba(255, 251, 241, 0.98); border-color: rgba(214, 155, 59, 0.36);
                |            box-shadow: 0 0 0 1px rgba(214, 155, 59, 0.14), 0 0 18px rgba(214, 155, 59, 0.18);
                |        }
                |        .clock.expired {
                |            background: rgba(105, 39, 31, 0.94); border-color: rgba(201, 95, 72, 0.5);
                |            box-shadow: 0 0 0 1px rgba(201, 95, 72, 0.2), 0 0 20px rgba(201, 95, 72, 0.18);
                |        }
                |        .board {
                |            width: 100%; aspect-ratio: 1; display: grid;
                |            grid-template-columns: repeat(8, minmax(0, 1fr)); grid-template-rows: repeat(8, minmax(0, 1fr));
                |            border: 0.6rem solid #2b281f; border-radius: 0.35rem;
                |            overflow: hidden; box-shadow: 0 24px 55px -26px rgba(0,0,0,0.86);
                |            align-content: start;
                |            line-height: 0;
                |        }
                |        .square {
                |            width: 100%; height: auto; aspect-ratio: 1; display: flex; justify-content: center; align-items: center;
                |            font-size: clamp(2rem, 5.4vw, 4.15rem); cursor: pointer; user-select: none;
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
                |        .sq-light .coord-label { color: rgba(67, 57, 38, 0.82); }
                |        .sq-dark .coord-label { color: rgba(246, 236, 213, 0.9); }
                |        .modal-overlay {
                |            position: fixed; top: 0; left: 0; width: 100vw; height: 100vh;
                |            background: rgba(0,0,0,0.6); backdrop-filter: blur(4px);
                |            display: flex; justify-content: center; align-items: center;
                |            z-index: 50; opacity: 0; pointer-events: none; transition: opacity 0.2s ease;
                |        }
                |        .modal-overlay.active { opacity: 1; pointer-events: all; }
                |        .modal-content {
                |            background: var(--surface); border: 1px solid var(--border);
                |            border-radius: 1rem; padding: 2rem; display: flex; gap: 1rem;
                |            box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.7);
                |        }
                |        .promo-btn {
                |            background: var(--surface-2); border: 1px solid var(--border); border-radius: 0.5rem;
                |            font-size: 2.5rem; width: 4.5rem; height: 4.5rem; cursor: pointer; color: var(--text-color); transition: all 0.2s;
                |        }
                |        .promo-btn:hover { background: var(--highlight); transform: translateY(-2px); }
                |        .piece-white { color: #ffffff; text-shadow: 0 2px 4px rgba(0,0,0,0.6); }
                |        .piece-black { color: #000000; text-shadow: 0 1px 2px rgba(255,255,255,0.6); }
                |        .board.game-over { pointer-events: none; opacity: 0.75; }
                |        .board.locked { pointer-events: none; opacity: 0.8; }
                |        .action-bar { display: flex; gap: 0.75rem; align-items: center; flex-wrap: wrap; justify-content: center; }
                |        .action-btn {
                |            min-height: 2.65rem; padding: 0.55rem 1rem; border-radius: 0.5rem; border: 1px solid var(--border);
                |            background: var(--surface-2); color: var(--text-color); font-size: 0.9rem; font-weight: 700;
                |            cursor: pointer; transition: all 0.2s;
                |        }
                |        .action-btn:hover:not(:disabled) { background: var(--highlight); }
                |        .action-btn:disabled { opacity: 0.35; cursor: not-allowed; }
                |        .action-btn.accept { background: rgba(124,159,82,0.28); border-color: rgba(124,159,82,0.58); }
                |        .action-btn.accept:hover:not(:disabled) { background: rgba(124,159,82,0.48); }
                |        .action-btn.decline { background: rgba(214,155,59,0.26); border-color: rgba(214,155,59,0.54); }
                |        .action-btn.decline:hover:not(:disabled) { background: rgba(214,155,59,0.45); }
                |        .action-btn.resign { background: rgba(201,95,72,0.26); border-color: rgba(201,95,72,0.56); }
                |        .action-btn.resign:hover:not(:disabled) { background: rgba(201,95,72,0.46); }
                |        .action-btn.new-game { background: rgba(105,127,79,0.28); border-color: rgba(136,157,94,0.5); }
                |        .action-btn.new-game:hover:not(:disabled) { background: rgba(105,127,79,0.46); }
                |        .captured-pieces { font-size: 1.15rem; min-height: 1.35rem; letter-spacing: 2px; color: var(--text-color); margin: 0; }
                |        .side-nav {
                |            position: sticky; top: 4.5rem; min-height: calc(100vh - 4.5rem);
                |            background: rgba(28, 27, 22, 0.94); border: 1px solid var(--border);
                |            border-left: 0; border-top: 0; border-radius: 0 0.45rem 0.45rem 0;
                |            padding: 0.9rem 0.8rem; display: flex; flex-direction: column; gap: 1rem;
                |        }
                |        .sidebar {
                |            background: rgba(36, 35, 29, 0.92); border: 1px solid var(--border); border-radius: 0.45rem; padding: 1rem;
                |            display: flex; flex-direction: column; gap: 1rem; min-width: 0; width: 100%;
                |            min-height: calc(100vh - 7rem); box-shadow: 0 18px 42px -30px rgba(0, 0, 0, 0.8);
                |            position: sticky; top: 6rem;
                |        }
                |        .tab-bar {
                |            display: grid; grid-template-columns: 1fr; gap: 0.35rem;
                |        }
                |        .tab-btn {
                |            border: 0; border-radius: 0.35rem; padding: 0.78rem 0.8rem; text-align: left;
                |            background: transparent; color: var(--muted); cursor: pointer; transition: all 0.2s ease;
                |            display: flex; align-items: center; gap: 0.7rem; min-height: 3.1rem;
                |        }
                |        .tab-btn:hover { background: rgba(255,255,255,0.06); color: #f8fafc; }
                |        .tab-btn.active {
                |            background: rgba(214, 155, 59, 0.22);
                |            color: #fff6df; box-shadow: inset 0 0 0 1px rgba(214, 155, 59, 0.22);
                |        }
                |        .tab-icon {
                |            width: 2rem; height: 2rem; border-radius: 0.35rem; display: grid; place-items: center;
                |            background: rgba(255,255,255,0.06); color: #f2ddb5; font-size: 0.78rem; font-weight: 900;
                |            flex: 0 0 auto;
                |        }
                |        .tab-label { font-size: 0.98rem; font-weight: 800; }
                |        .sr-only {
                |            position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px;
                |            overflow: hidden; clip: rect(0, 0, 0, 0); white-space: nowrap; border: 0;
                |        }
                |        .tab-panel { display: none; flex-direction: column; gap: 1rem; }
                |        .tab-panel.active { display: flex; }
                |        .panel-card {
                |            background: rgba(48, 46, 37, 0.72); border: 1px solid var(--border);
                |            border-radius: 0.45rem; padding: 1rem; display: flex; flex-direction: column; gap: 0.8rem;
                |        }
                |        .panel-card h3 { margin: 0; font-size: 1.05rem; }
                |        .mini-title { display: flex; align-items: center; gap: 0.5rem; margin: 0; font-size: 1rem; }
                |        .mini-icon {
                |            width: 1.8rem; height: 1.8rem; border-radius: 0.35rem; display: grid; place-items: center;
                |            background: rgba(214,155,59,0.14); color: #f2ddb5; font-size: 0.72rem; font-weight: 900;
                |        }
                |        .move-list {
                |            height: clamp(12rem, 30vh, 22rem); overflow-y: auto; background: rgba(23,22,18,0.48); border-radius: 0.35rem;
                |            padding: 0.6rem; font-family: monospace; font-size: 0.95rem; border: 1px solid var(--border);
                |        }
                |        .move-list div { padding: 0.45rem 0.55rem; border-radius: 0.45rem; border-bottom: 1px solid rgba(255,255,255,0.05); }
                |        .move-list div:last-child { border-bottom: 0; }
                |        .move-list .latest { background: rgba(124,159,82,0.18); border: 1px solid rgba(124,159,82,0.24); }
                |        .action-grid { display: grid; grid-template-columns: repeat(2, minmax(0, 1fr)); gap: 0.75rem; }
                |        .action-grid .action-btn.wide { grid-column: 1 / -1; }
                |        .notation-box { display: flex; flex-direction: column; gap: 0.75rem; }
                |        .field-group { display: flex; flex-direction: column; gap: 0.45rem; }
                |        .field-group label { font-size: 0.85rem; font-weight: 700; color: #eadfc7; }
                |        .hint { font-size: 0.8rem; color: var(--muted); }
                |        .notation-text {
                |            width: 100%; min-height: 9rem; background: rgba(23,22,18,0.45); color: var(--text-color);
                |            border: 1px solid var(--border); border-radius: 0.35rem; padding: 0.75rem;
                |            font-family: monospace; resize: vertical; box-sizing: border-box;
                |        }
                |        select, .text-input {
                |            width: 100%;
                |            background: rgba(23,22,18,0.45); color: var(--text-color); border: 1px solid var(--border);
                |            padding: 0.7rem 0.75rem; border-radius: 0.35rem;
                |        }
                |        .text-input::placeholder { color: rgba(184, 173, 150, 0.72); }
                |        .future-controls { display: grid; gap: 0.75rem; }
                |        .future-controls .action-btn { width: 100%; }
                |        .challenge-meta {
                |            padding: 0.65rem 0.75rem; border-radius: 0.65rem; min-height: 1.2rem;
                |            background: rgba(23,22,18,0.42); border: 1px solid var(--border);
                |            color: #eadfc7; font-size: 0.95rem; font-weight: 700;
                |        }
                |        .statistics-table {
                |            width: 100%; border-collapse: collapse; font-variant-numeric: tabular-nums;
                |        }
                |        .statistics-table th, .statistics-table td {
                |            padding: 0.7rem 0.45rem; text-align: right; border-bottom: 1px solid rgba(255,255,255,0.07);
                |        }
                |        .statistics-table th:first-child, .statistics-table td:first-child { text-align: left; }
                |        .statistics-table th { color: var(--muted); font-size: 0.75rem; text-transform: uppercase; }
                |        .statistics-table tbody tr:first-child td { color: #e0b04b; font-weight: 800; }
                |        .statistics-empty { color: var(--muted); line-height: 1.5; }
                |        .tournament-list, .tournament-log {
                |            min-height: 6rem; max-height: 14rem; overflow: auto; white-space: pre-wrap;
                |            background: rgba(23,22,18,0.48); border: 1px solid var(--border);
                |            border-radius: 0.35rem; padding: 0.7rem; color: #eadfc7;
                |            font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; font-size: 0.82rem; line-height: 1.45;
                |        }
                |        .tournament-list {
                |            display: grid; gap: 0.6rem; white-space: normal; font-family: inherit;
                |        }
                |        .tournament-entry {
                |            padding: 0.65rem; border: 1px solid rgba(255,255,255,0.08); border-radius: 0.35rem;
                |            background: rgba(255,255,255,0.035); cursor: pointer;
                |        }
                |        .tournament-entry:hover { border-color: rgba(216,189,119,0.36); background: rgba(255,255,255,0.055); }
                |        .tournament-entry-header {
                |            display: flex; justify-content: space-between; gap: 0.75rem; align-items: flex-start;
                |        }
                |        .tournament-name { font-weight: 800; color: #fff5dd; overflow-wrap: anywhere; }
                |        .tournament-id { margin-top: 0.15rem; color: #c9bfa7; font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; }
                |        .tournament-status {
                |            flex: 0 0 auto; color: #15130f; background: #d8bd77; border-radius: 999px;
                |            padding: 0.12rem 0.45rem; font-size: 0.72rem; font-weight: 900; text-transform: uppercase;
                |        }
                |        .tournament-meta { margin-top: 0.45rem; color: var(--muted); font-size: 0.78rem; }
                |        .tournament-players, .tournament-games {
                |            margin-top: 0.6rem; padding-top: 0.55rem; border-top: 1px solid rgba(255,255,255,0.08);
                |            display: grid; gap: 0.4rem; color: #eadfc7; font-size: 0.82rem;
                |        }
                |        .tournament-player, .tournament-game {
                |            display: flex; justify-content: space-between; gap: 0.6rem;
                |            font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
                |        }
                |        .tournament-player-name { overflow-wrap: anywhere; }
                |        .tournament-player-score { color: var(--muted); flex: 0 0 auto; }
                |        .tournament-game {
                |            width: 100%; border: 1px solid rgba(255,255,255,0.08); border-radius: 0.35rem;
                |            padding: 0.6rem 0.65rem; background: rgba(23,22,18,0.38); color: #fff5dd; cursor: pointer; font-weight: 800;
                |        }
                |        .tournament-game:hover { border-color: rgba(216,189,119,0.4); background: rgba(216,189,119,0.12); }
                |        .tournament-game span:last-child { color: var(--muted); flex: 0 0 auto; }
                |        .tournament-list button {
                |            margin-top: 0.45rem; width: 100%;
                |        }
                |        .start-btn {
                |            width: 100%; padding: 0.95rem 1.2rem; font-size: 1.15rem; font-weight: 700;
                |            background: linear-gradient(180deg, rgba(124, 159, 82, 0.96), rgba(86, 119, 67, 0.96));
                |            border-color: rgba(164, 190, 104, 0.45); color: #fff8e8;
                |        }
                |        .start-btn:disabled { opacity: 0.55; }
                |        @media (min-width: 1121px) {
                |            .clock-row { right: -7.8rem; }
                |        }
                |        @media (max-width: 1120px) {
                |            .main-layout, .main-layout.panel-only { grid-template-columns: 1fr; }
                |            .main-layout.panel-only .sidebar { grid-column: auto; max-width: none; }
                |            .side-nav { position: static; min-height: auto; }
                |            .board-area { min-height: auto; padding-top: 0.5rem; }
                |            .board-panel { width: min(100%, 42rem); min-width: 0; }
                |            .sidebar { position: static; min-height: auto; }
                |        }
                |        @media (max-width: 640px) {
                |            .topbar { position: static; align-items: flex-start; flex-direction: column; gap: 0.75rem; }
                |            .status { font-size: 0.9rem; }
                |            .main-layout { width: 100%; gap: 1rem; padding: 0.75rem; }
                |            .board-area { padding: 0; }
                |            .board { border-width: 0.35rem; }
                |            .square { font-size: clamp(1.75rem, 9vw, 2.4rem); min-width: 0; }
                |            .sidebar { min-width: 0; width: 100%; padding: 0.8rem; }
                |            .statistics-table th, .statistics-table td { padding: 0.55rem 0.2rem; font-size: 0.75rem; }
                |            .action-grid, .tab-bar { grid-template-columns: 1fr; }
                |        }
                |    </style>
                |</head>
                |<body>
                |<div class="app-shell">
                |<header class="topbar">
                |    <div class="brand">
                |        <h1>TiChess</h1>
                |        <span class="brand-subtitle">Web Chess Console</span>
                |    </div>
                |    <div id="status" class="status">Connecting...</div>
                |</header>
                |<main id="main-layout" class="main-layout">
                |<nav class="side-nav" aria-label="Hauptnavigation">
                |    <div class="tab-bar" role="tablist" aria-label="Schach Seitenbereich">
                |        <button id="tab-btn-game" class="tab-btn active" type="button" role="tab" aria-selected="true" aria-controls="tab-game" onclick="switchTab('game')">
                |            <span class="tab-icon" aria-hidden="true">SP</span>
                |            <span class="tab-label">Spiel</span>
                |        </button>
                |        <button id="tab-btn-io" class="tab-btn" type="button" role="tab" aria-selected="false" aria-controls="tab-io" onclick="switchTab('io')">
                |            <span class="tab-icon" aria-hidden="true">PGN</span>
                |            <span class="tab-label">Notation</span>
                |        </button>
                |        <button id="tab-btn-challenges" class="tab-btn" type="button" role="tab" aria-selected="false" aria-controls="tab-challenges" onclick="switchTab('challenges')">
                |            <span class="tab-icon" aria-hidden="true">TR</span>
                |            <span class="tab-label">Training</span>
                |        </button>
                |        <button id="tab-btn-tournament" class="tab-btn" type="button" role="tab" aria-selected="false" aria-controls="tab-tournament" onclick="switchTab('tournament')">
                |            <span class="tab-icon" aria-hidden="true">TN</span>
                |            <span class="tab-label">Turnier</span>
                |        </button>
                |        <button id="tab-btn-statistics" class="tab-btn" type="button" role="tab" aria-selected="false" aria-controls="tab-statistics" onclick="switchTab('statistics')">
                |            <span class="tab-icon" aria-hidden="true">ST</span>
                |            <span class="tab-label">Statistik</span>
                |        </button>
                |    </div>
                |</nav>
                |<section class="board-area" aria-label="Schachbrett">
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
                |</section>
                |<div class="sidebar">
                |    <section id="tab-game" class="tab-panel active" role="tabpanel" aria-labelledby="tab-btn-game">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">01</span><span>Start</span></h3>
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
                |            <h3 class="mini-title"><span class="mini-icon">02</span><span>Aktionen</span></h3>
                |            <div class="action-grid">
                |                <button id="btn-draw" class="action-btn" onclick="sendCommand('draw')">Remis anbieten</button>
                |                <button id="btn-resign" class="action-btn resign" onclick="sendCommand('resign')">Aufgeben</button>
                |                <button id="btn-accept" class="action-btn accept wide" style="display:none" onclick="sendCommand('accept')">Remis annehmen</button>
                |                <button id="btn-decline" class="action-btn decline wide" style="display:none" onclick="sendCommand('decline')">Remis ablehnen</button>
                |                <button id="btn-new" class="action-btn new-game wide" onclick="sendCommand('new')">Neues Spiel</button>
                |            </div>
                |        </div>
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">03</span><span>Zughistorie</span></h3>
                |            <div id="move-list" class="move-list"></div>
                |        </div>
                |    </section>
                |    <section id="tab-io" class="tab-panel" role="tabpanel" aria-labelledby="tab-btn-io">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">PGN</span><span>Notation</span></h3>
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
                |    <section id="tab-tournament" class="tab-panel" role="tabpanel" aria-labelledby="tab-btn-tournament">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span class="mini-icon">TN</span><span>Turnier</span></h3>
                |            <div class="future-controls">
                |                <div class="field-group">
                |                    <label for="tournament-base-url">Server</label>
                |                    <input id="tournament-base-url" class="text-input" value="https://tournament.staging.maichess.berger-software.com">
                |                </div>
                |                <div class="field-group">
                |                    <label for="tournament-token">Token</label>
                |                    <input id="tournament-token" class="text-input" type="password" placeholder="TOURNAMENT_TOKEN">
                |                </div>
                |                <div class="field-group">
                |                    <label for="tournament-id">Turnier-ID</label>
                |                    <input id="tournament-id" class="text-input" placeholder="tournament id">
                |                </div>
                |                <div class="action-grid">
                |                    <button class="action-btn" type="button" onclick="loadTournamentList()">Aktualisieren</button>
                |                    <button class="action-btn" type="button" onclick="connectTournamentStream()">Turnier verfolgen</button>
                |                    <button class="action-btn resign wide" type="button" onclick="stopTournamentStreams()">Streams stoppen</button>
                |                </div>
                |                <div id="tournament-list" class="tournament-list">Noch keine Turnierliste geladen.</div>
                |            </div>
                |        </div>
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span>Spiel verfolgen</span></h3>
                |            <div class="future-controls">
                |                <div class="field-group">
                |                    <label for="tournament-game-id">Game-ID</label>
                |                    <input id="tournament-game-id" class="text-input" placeholder="game id aus dem Stream">
                |                </div>
                |                <button class="action-btn start-btn" type="button" onclick="connectTournamentGameStream()">Spielstream öffnen</button>
                |                <div id="tournament-log" class="tournament-log">Bereit.</div>
                |            </div>
                |        </div>
                |    </section>
                |    <section id="tab-statistics" class="tab-panel" role="tabpanel" aria-labelledby="tab-btn-statistics">
                |        <div class="panel-card">
                |            <h3 class="mini-title"><span>Bestenliste</span></h3>
                |            <table class="statistics-table">
                |                <thead>
                |                    <tr><th>Farbe</th><th>Spiele</th><th>Siege</th><th>Remis</th><th>Niederl.</th><th>Punkte</th></tr>
                |                </thead>
                |                <tbody id="statistics-body">
                |                    <tr><td colspan="6" class="statistics-empty">Noch keine abgeschlossene Partie ausgewertet.</td></tr>
                |                </tbody>
                |            </table>
                |        </div>
                |    </section>
                |</div>
                |</main>
                |
                |<div id="promo-modal" class="modal-overlay">
                |    <div class="modal-content">
                |        <button class="promo-btn" onclick="selectPromotion('q')">&#x265B;&#xFE0E;</button>
                |        <button class="promo-btn" onclick="selectPromotion('r')">&#x265C;&#xFE0E;</button>
                |        <button class="promo-btn" onclick="selectPromotion('b')">&#x265D;&#xFE0E;</button>
                |        <button class="promo-btn" onclick="selectPromotion('n')">&#x265E;&#xFE0E;</button>
                |    </div>
                |</div>
                |</div>
                |
                |""".stripMargin +
                """<script>
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
                |    let tournamentAbortController = null;
                |    let tournamentGameAbortController = null;
                |    let gameStarted = false;
                |    let gameTimedOut = false;
                |    let timeoutLoser = null;
                |    let clockBaseMs = 300000;
                |    let whiteRemainingMs = clockBaseMs;
                |    let blackRemainingMs = clockBaseMs;
                |    let activeClock = null;
                |    let timerHandle = null;
                |    let statisticsTimerHandle = null;
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
                |    async function fetchWithTimeout(url, options = {}, timeoutMs = 2500) {
                |        const controller = new AbortController();
                |        const timeout = setTimeout(() => controller.abort(), timeoutMs);
                |        try {
                |            return await fetch(url, { ...options, signal: controller.signal });
                |        } finally {
                |            clearTimeout(timeout);
                |        }
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
                |        ['game', 'io', 'challenges', 'tournament', 'statistics'].forEach(tab => {
                |            const active = tabName === tab;
                |            document.getElementById('tab-btn-' + tab).classList.toggle('active', active);
                |            document.getElementById('tab-btn-' + tab).setAttribute('aria-selected', String(active));
                |            document.getElementById('tab-' + tab).classList.toggle('active', active);
                |        });
                |        document.getElementById('main-layout').classList.toggle('panel-only', tabName === 'tournament' || tabName === 'statistics');
                |        if (tabName === 'statistics') startStatisticsPolling(); else stopStatisticsPolling();
                |    }
                |    function startStatisticsPolling() {
                |        fetchStatistics();
                |        if (statisticsTimerHandle === null) statisticsTimerHandle = setInterval(fetchStatistics, 10000);
                |    }
                |    function stopStatisticsPolling() {
                |        if (statisticsTimerHandle !== null) {
                |            clearInterval(statisticsTimerHandle);
                |            statisticsTimerHandle = null;
                |        }
                |    }
                |    async function fetchStatistics() {
                |        const body = document.getElementById('statistics-body');
                |        try {
                |            const response = await fetchWithTimeout('/api/view/statistics', {}, 2000);
                |            const statistics = await response.json();
                |            body.innerHTML = '';
                |            if (!statistics.length) {
                |                body.innerHTML = '<tr><td colspan="6" class="statistics-empty">Noch keine abgeschlossene Partie ausgewertet.</td></tr>';
                |                return;
                |            }
                |            statistics.forEach(entry => {
                |                const row = document.createElement('tr');
                |                const label = entry.player === 'White' ? 'Weiß' : entry.player === 'Black' ? 'Schwarz' : entry.player;
                |                [label, entry.games, entry.victories, entry.draws, entry.losses, entry.score].forEach(value => {
                |                    const cell = document.createElement('td');
                |                    cell.innerText = value;
                |                    row.appendChild(cell);
                |                });
                |                body.appendChild(row);
                |            });
                |        } catch (error) {
                |            body.innerHTML = '<tr><td colspan="6" class="statistics-empty">Statistik ist derzeit nicht erreichbar.</td></tr>';
                |        }
                |    }
                |    function tournamentConfig() {
                |        return {
                |            baseUrl: document.getElementById('tournament-base-url').value.trim(),
                |            token: document.getElementById('tournament-token').value.trim(),
                |            tournamentId: document.getElementById('tournament-id').value.trim(),
                |            gameId: document.getElementById('tournament-game-id').value.trim()
                |        };
                |    }
                |    function appendTournamentLog(text) {
                |        const log = document.getElementById('tournament-log');
                |        const timestamp = new Date().toLocaleTimeString();
                |        log.textContent = (log.textContent === 'Bereit.' ? '' : log.textContent + '\n') + `[${timestamp}] ${text}`;
                |        log.scrollTop = log.scrollHeight;
                |    }
                |    function renderTournamentPlayers(entry, tournament) {
                |        entry.querySelectorAll('.tournament-players').forEach(node => node.remove());
                |        const panel = document.createElement('div');
                |        panel.className = 'tournament-players';
                |        const players = (tournament.standing && tournament.standing.players) || tournament.players || tournament.participants || [];
                |        if (!Array.isArray(players) || players.length === 0) {
                |            panel.textContent = 'Noch keine angemeldeten Spieler gefunden.';
                |            entry.appendChild(panel);
                |            return;
                |        }
                |        players.forEach(player => {
                |            const bot = player.bot || player;
                |            const row = document.createElement('div');
                |            row.className = 'tournament-player';
                |            const name = document.createElement('span');
                |            name.className = 'tournament-player-name';
                |            name.textContent = bot.name || bot.id || player.name || player.id || 'Bot';
                |            const score = document.createElement('span');
                |            score.className = 'tournament-player-score';
                |            score.textContent = [player.rank ? `#${player.rank}` : null, player.points !== undefined ? `${player.points} P` : null, bot.id || null].filter(Boolean).join(' · ');
                |            row.appendChild(name);
                |            row.appendChild(score);
                |            panel.appendChild(row);
                |        });
                |        entry.appendChild(panel);
                |    }
                |    function renderTournamentGames(entry, roundData) {
                |        entry.querySelectorAll('.tournament-games').forEach(node => node.remove());
                |        const panel = document.createElement('div');
                |        panel.className = 'tournament-games';
                |        const pairings = roundData.pairings || [];
                |        if (!Array.isArray(pairings) || pairings.length === 0) {
                |            panel.textContent = 'Noch keine Spiele fuer diese Runde gefunden.';
                |            entry.appendChild(panel);
                |            return;
                |        }
                |        pairings.forEach(pairing => {
                |            const gameId = pairing.gameId || (pairing.matchResults && pairing.matchResults[0] && pairing.matchResults[0].gameId);
                |            const white = (pairing.white && (pairing.white.name || pairing.white.id)) || 'White';
                |            const black = (pairing.black && (pairing.black.name || pairing.black.id)) || 'Black';
                |            const button = document.createElement('button');
                |            button.className = 'tournament-game';
                |            button.type = 'button';
                |            button.disabled = !gameId;
                |            const label = document.createElement('span');
                |            label.textContent = `${white} vs ${black}`;
                |            const id = document.createElement('span');
                |            id.textContent = gameId ? `Game ${gameId}` : 'Keine Game-ID';
                |            button.appendChild(label);
                |            button.appendChild(id);
                |            if (gameId) {
                |                button.onclick = event => {
                |                    event.stopPropagation();
                |                    document.getElementById('tournament-game-id').value = gameId;
                |                    document.getElementById('main-layout').classList.remove('panel-only');
                |                    connectTournamentGameStream();
                |                };
                |            }
                |            panel.appendChild(button);
                |        });
                |        entry.appendChild(panel);
                |    }
                |    async function loadTournamentRound(tournamentId, round, entry) {
                |        const cfg = tournamentConfig();
                |        if (!cfg.baseUrl || !tournamentId || !round) return;
                |        try {
                |            const res = await fetchWithTimeout('/api/tournament/round', {
                |                method: 'POST',
                |                headers: { 'Content-Type': 'application/json' },
                |                body: JSON.stringify({ baseUrl: cfg.baseUrl, token: cfg.token ? cfg.token : null, tournamentId, round })
                |            }, 5000);
                |            const data = await res.json();
                |            if (data.success) renderTournamentGames(entry, JSON.parse(data.body));
                |        } catch (_) {}
                |    }
                |    async function loadTournamentDetails(tournamentId, entry) {
                |        const cfg = tournamentConfig();
                |        if (!cfg.baseUrl || !tournamentId) return;
                |        entry.querySelectorAll('.tournament-players, .tournament-games').forEach(node => node.remove());
                |        const loading = document.createElement('div');
                |        loading.className = 'tournament-players';
                |        loading.textContent = 'Lade Spieler und Spiele...';
                |        entry.appendChild(loading);
                |        try {
                |            const res = await fetchWithTimeout('/api/tournament/detail', {
                |                method: 'POST',
                |                headers: { 'Content-Type': 'application/json' },
                |                body: JSON.stringify({ baseUrl: cfg.baseUrl, token: cfg.token ? cfg.token : null, tournamentId })
                |            }, 5000);
                |            const data = await res.json();
                |            if (!data.success) {
                |                loading.textContent = data.error || data.body || 'Turnierdetails konnten nicht geladen werden.';
                |                return;
                |            }
                |            const tournament = JSON.parse(data.body);
                |            renderTournamentPlayers(entry, tournament);
                |            loadTournamentRound(tournamentId, tournament.round || 1, entry);
                |        } catch (error) {
                |            loading.textContent = 'Turnierdetails sind nicht erreichbar.';
                |        }
                |    }
                |    function renderTournamentListBody(body) {
                |        const target = document.getElementById('tournament-list');
                |        try {
                |            const parsed = JSON.parse(body);
                |            const groupedItems = [];
                |            if (Array.isArray(parsed)) {
                |                parsed.forEach(item => groupedItems.push({ item, status: item.status || '' }));
                |            } else {
                |                Object.entries(parsed || {}).forEach(([status, value]) => {
                |                    if (Array.isArray(value)) {
                |                        value.forEach(item => groupedItems.push({ item, status: item.status || status }));
                |                    }
                |                });
                |            }
                |            if (groupedItems.length > 0) {
                |                target.innerHTML = '';
                |                groupedItems.forEach(({ item, status }) => {
                |                    const id = item.id || item.tournamentId || item._id || '';
                |                    const name = item.fullName || item.name || item.title || id || 'Turnier';
                |                    const playerCount = item.nbPlayers || item.players || item.playerCount;
                |                    const roundCount = item.nbRounds || item.rounds;
                |                    const clock = item.clock && item.clock.limit ? `${item.clock.limit}+${item.clock.increment || 0}` : null;
                |                    const meta = [
                |                        item.format,
                |                        item.variant,
                |                        roundCount ? `${roundCount} Runden` : null,
                |                        playerCount ? `${playerCount} Spieler` : null,
                |                        clock ? `${clock} Uhr` : null,
                |                        item.rated === true ? 'rated' : (item.rated === false ? 'casual' : null)
                |                    ].filter(Boolean).join(' • ');
                |                    const entry = document.createElement('div');
                |                    entry.className = 'tournament-entry';
                |                    if (id) {
                |                        entry.onclick = () => {
                |                            document.getElementById('tournament-id').value = id;
                |                            loadTournamentDetails(id, entry);
                |                        };
                |                    }
                |                    const header = document.createElement('div');
                |                    header.className = 'tournament-entry-header';
                |                    const titleBlock = document.createElement('div');
                |                    const nameEl = document.createElement('div');
                |                    nameEl.className = 'tournament-name';
                |                    nameEl.textContent = name;
                |                    titleBlock.appendChild(nameEl);
                |                    if (id) {
                |                        const idEl = document.createElement('div');
                |                        idEl.className = 'tournament-id';
                |                        idEl.textContent = `ID: ${id}`;
                |                        titleBlock.appendChild(idEl);
                |                    }
                |                    header.appendChild(titleBlock);
                |                    if (status) {
                |                        const statusEl = document.createElement('div');
                |                        statusEl.className = 'tournament-status';
                |                        statusEl.textContent = status;
                |                        header.appendChild(statusEl);
                |                    }
                |                    entry.appendChild(header);
                |                    if (meta) {
                |                        const metaEl = document.createElement('div');
                |                        metaEl.className = 'tournament-meta';
                |                        metaEl.textContent = meta;
                |                        entry.appendChild(metaEl);
                |                    }
                |                    if (id) {
                |                        const button = document.createElement('button');
                |                        button.className = 'action-btn';
                |                        button.type = 'button';
                |                        button.textContent = 'Verfolgen';
                |                        button.onclick = event => {
                |                            event.stopPropagation();
                |                            document.getElementById('tournament-id').value = id;
                |                            connectTournamentStream();
                |                        };
                |                        entry.appendChild(button);
                |                    }
                |                    target.appendChild(entry);
                |                });
                |                return;
                |            }
                |            target.textContent = JSON.stringify(parsed, null, 2);
                |        } catch (_) {
                |            target.textContent = body || 'Keine Turnierliste empfangen.';
                |        }
                |    }
                |    async function loadTournamentList() {
                |        const cfg = tournamentConfig();
                |        if (!cfg.baseUrl) {
                |            document.getElementById('tournament-list').textContent = 'Bitte Server-URL eintragen.';
                |            return;
                |        }
                |        document.getElementById('tournament-list').textContent = 'Lade Turniere...';
                |        try {
                |            const res = await fetchWithTimeout('/api/tournament/list', {
                |                method: 'POST',
                |                headers: { 'Content-Type': 'application/json' },
                |                body: JSON.stringify({ baseUrl: cfg.baseUrl, token: cfg.token ? cfg.token : null })
                |            }, 5000);
                |            const data = await res.json();
                |            if (!data.success) {
                |                document.getElementById('tournament-list').textContent = data.error || data.body || 'Turnierliste konnte nicht geladen werden.';
                |                return;
                |            }
                |            renderTournamentListBody(data.body);
                |        } catch (error) {
                |            document.getElementById('tournament-list').textContent = 'Turnierliste ist nicht erreichbar.';
                |        }
                |    }
                |    async function readNdjsonStream(response, onEvent) {
                |        const reader = response.body.getReader();
                |        const decoder = new TextDecoder();
                |        let buffer = '';
                |        while (true) {
                |            const { value, done } = await reader.read();
                |            if (done) break;
                |            buffer += decoder.decode(value, { stream: true });
                |            const lines = buffer.split('\n');
                |            buffer = lines.pop();
                |            lines.map(line => line.trim()).filter(Boolean).forEach(line => {
                |                try { onEvent(JSON.parse(line), line); } catch (_) { onEvent(null, line); }
                |            });
                |        }
                |        if (buffer.trim()) {
                |            try { onEvent(JSON.parse(buffer.trim()), buffer.trim()); } catch (_) { onEvent(null, buffer.trim()); }
                |        }
                |    }
                |    async function connectTournamentStream() {
                |        const cfg = tournamentConfig();
                |        if (!cfg.baseUrl || !cfg.token || !cfg.tournamentId) {
                |            appendTournamentLog('Server, Token und Turnier-ID werden benoetigt.');
                |            return;
                |        }
                |        if (tournamentAbortController) tournamentAbortController.abort();
                |        tournamentAbortController = new AbortController();
                |        appendTournamentLog(`Verbinde Turnier ${cfg.tournamentId}...`);
                |        try {
                |            const response = await fetch('/api/tournament/stream', {
                |                method: 'POST',
                |                headers: { 'Content-Type': 'application/json' },
                |                body: JSON.stringify({ baseUrl: cfg.baseUrl, token: cfg.token, tournamentId: cfg.tournamentId }),
                |                signal: tournamentAbortController.signal
                |            });
                |            if (!response.ok) {
                |                appendTournamentLog(`Turnierstream fehlgeschlagen (${response.status}).`);
                |                return;
                |            }
                |            await readNdjsonStream(response, event => {
                |                if (!event) return;
                |                const gameInfo = event.gameId ? ` game=${event.gameId}` : '';
                |                const roundInfo = event.round ? ` round=${event.round}` : '';
                |                const colorInfo = event.color ? ` color=${event.color}` : '';
                |                appendTournamentLog(`${event.type}${roundInfo}${gameInfo}${colorInfo}`);
                |                if (event.gameId) document.getElementById('tournament-game-id').value = event.gameId;
                |            });
                |        } catch (error) {
                |            if (error.name !== 'AbortError') appendTournamentLog('Turnierstream getrennt.');
                |        }
                |    }
                |    async function connectTournamentGameStream() {
                |        const cfg = tournamentConfig();
                |        if (!cfg.baseUrl || !cfg.token || !cfg.tournamentId || !cfg.gameId) {
                |            appendTournamentLog('Server, Token, Turnier-ID und Game-ID werden benoetigt.');
                |            return;
                |        }
                |        if (tournamentGameAbortController) tournamentGameAbortController.abort();
                |        tournamentGameAbortController = new AbortController();
                |        appendTournamentLog(`Verbinde Spiel ${cfg.gameId}...`);
                |        try {
                |            const response = await fetch('/api/tournament/game-stream', {
                |                method: 'POST',
                |                headers: { 'Content-Type': 'application/json' },
                |                body: JSON.stringify({ baseUrl: cfg.baseUrl, token: cfg.token, tournamentId: cfg.tournamentId, gameId: cfg.gameId }),
                |                signal: tournamentGameAbortController.signal
                |            });
                |            if (!response.ok) {
                |                appendTournamentLog(`Spielstream fehlgeschlagen (${response.status}).`);
                |                return;
                |            }
                |            await readNdjsonStream(response, event => {
                |                if (!event) return;
                |                appendTournamentLog(`${event.type}${event.status ? ' status=' + event.status : ''}${event.winner ? ' winner=' + event.winner : ''}${event.uci ? ' move=' + event.uci : ''}`);
                |                if (event.fen) {
                |                    renderFen(event.fen);
                |                    serverStatusText = `Turnier-Spiel ${cfg.gameId}: ${event.status || event.type}`;
                |                    currentSideToMove = (event.fen.split(' ')[1] || 'w').trim();
                |                    updateStatusText();
                |                }
                |            });
                |        } catch (error) {
                |            if (error.name !== 'AbortError') appendTournamentLog('Spielstream getrennt.');
                |        }
                |    }
                |    function stopTournamentStreams() {
                |        if (tournamentAbortController) tournamentAbortController.abort();
                |        if (tournamentGameAbortController) tournamentGameAbortController.abort();
                |        tournamentAbortController = null;
                |        tournamentGameAbortController = null;
                |        appendTournamentLog('Streams gestoppt.');
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
                |            const response = await fetchWithTimeout('/api/view/game', {}, 1800); const data = await response.json();
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
                |            const res = await fetchWithTimeout('/api/controller/update', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ input: algebraicMove }) }, 6000);
                |            const data = await res.json();
                |            if (!data.success && data.message) serverStatusText = "Illegal: " + data.message;
                |            if (data.success && data.message) transientStatusText = data.message;
                |            challengeHintVisible = false;
                |            fetchGame();
                |        } catch (e) { console.error(e); }
                |    }
                |    async function sendCommand(cmd, options = {}) {
                |        try {
                |            const res = await fetchWithTimeout('/api/controller/update', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ input: cmd }) }, 6000);
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
                |    renderFen('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1');
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
        pathPrefix("api" / "tournament") {
          post {
            concat(
              path("list") {
                entity(as[TournamentListRequest]) { req =>
                  onComplete(fetchTournamentList(req)) {
                    case Success(response) => complete(response)
                    case Failure(ex)       => complete(TournamentProxyResponse(success = false, 500, "", Some(ex.getMessage)))
                  }
                }
              },
              path("detail") {
                entity(as[TournamentDetailRequest]) { req =>
                  onComplete(fetchTournamentDetail(req)) {
                    case Success(response) => complete(response)
                    case Failure(ex)       => complete(TournamentProxyResponse(success = false, 500, "", Some(ex.getMessage)))
                  }
                }
              },
              path("round") {
                entity(as[TournamentRoundRequest]) { req =>
                  onComplete(fetchTournamentRound(req)) {
                    case Success(response) => complete(response)
                    case Failure(ex)       => complete(TournamentProxyResponse(success = false, 500, "", Some(ex.getMessage)))
                  }
                }
              },
              path("stream") {
                entity(as[TournamentStreamRequest]) { req =>
                  onComplete(proxyTournamentStream(req.baseUrl, req.token, req.tournamentId, None)) {
                    case Success(response) => complete(response)
                    case Failure(ex)       => complete(HttpResponse(StatusCodes.BadGateway, entity = ex.getMessage))
                  }
                }
              },
              path("game-stream") {
                entity(as[TournamentGameStreamRequest]) { req =>
                  onComplete(proxyTournamentStream(req.baseUrl, req.token, req.tournamentId, Some(req.gameId))) {
                    case Success(response) => complete(response)
                    case Failure(ex)       => complete(HttpResponse(StatusCodes.BadGateway, entity = ex.getMessage))
                  }
                }
              }
            )
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
              },
              path("statistics") {
                onComplete(controllerClient.fetchStatistics()) {
                  case Success(statistics) => complete(statistics)
                  case Failure(ex)         => failWith(ex)
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
    println("  GET  /api/view/statistics")
    Await.result(system.whenTerminated, Duration.Inf)

  private def cleanTournamentBaseUrl(baseUrl: String): String =
    baseUrl.trim.stripSuffix("/")

  private def tournamentHeaders(token: Option[String]): List[HttpHeader] =
    RawHeader("Accept", "application/x-ndjson") ::
      token.filter(_.trim.nonEmpty).map(value => Authorization(OAuth2BearerToken(value.trim))).toList

  private def tournamentListHeaders(token: Option[String]): List[HttpHeader] =
    RawHeader("Accept", "application/json") ::
      token.filter(_.trim.nonEmpty).map(value => Authorization(OAuth2BearerToken(value.trim))).toList

  private def fetchTournamentList(req: TournamentListRequest)(implicit
      system: ActorSystem[?],
      ec: ExecutionContext
  ): Future[TournamentProxyResponse] =
    val request = HttpRequest(
      uri = s"${cleanTournamentBaseUrl(req.baseUrl)}/api/tournament",
      headers = tournamentListHeaders(req.token)
    )
    Http().singleRequest(request).flatMap { response =>
      response.entity.toStrict(5.seconds).map { strict =>
        val body = strict.data.utf8String
        TournamentProxyResponse(response.status.isSuccess(), response.status.intValue(), body, if response.status.isSuccess() then None else Some(body))
      }
    }

  private def fetchTournamentDetail(req: TournamentDetailRequest)(implicit
      system: ActorSystem[?],
      ec: ExecutionContext
  ): Future[TournamentProxyResponse] =
    val request = HttpRequest(
      uri = s"${cleanTournamentBaseUrl(req.baseUrl)}/api/tournament/${req.tournamentId.trim}",
      headers = tournamentListHeaders(req.token)
    )
    Http().singleRequest(request).flatMap { response =>
      response.entity.toStrict(5.seconds).map { strict =>
        val body = strict.data.utf8String
        TournamentProxyResponse(response.status.isSuccess(), response.status.intValue(), body, if response.status.isSuccess() then None else Some(body))
      }
    }

  private def fetchTournamentRound(req: TournamentRoundRequest)(implicit
      system: ActorSystem[?],
      ec: ExecutionContext
  ): Future[TournamentProxyResponse] =
    val request = HttpRequest(
      uri = s"${cleanTournamentBaseUrl(req.baseUrl)}/api/tournament/${req.tournamentId.trim}/round/${req.round}",
      headers = tournamentListHeaders(req.token)
    )
    Http().singleRequest(request).flatMap { response =>
      response.entity.toStrict(5.seconds).map { strict =>
        val body = strict.data.utf8String
        TournamentProxyResponse(response.status.isSuccess(), response.status.intValue(), body, if response.status.isSuccess() then None else Some(body))
      }
    }

  private def proxyTournamentStream(
      baseUrl: String,
      token: String,
      tournamentId: String,
      gameId: Option[String]
  )(implicit
      system: ActorSystem[?],
      ec: ExecutionContext
  ): Future[HttpResponse] =
    val suffix = gameId match
      case Some(id) => s"/api/tournament/$tournamentId/game/$id/stream"
      case None     => s"/api/tournament/$tournamentId/stream"
    val request = HttpRequest(
      uri = s"${cleanTournamentBaseUrl(baseUrl)}$suffix",
      headers = tournamentHeaders(Some(token))
    )
    Http().singleRequest(request).flatMap { response =>
      if response.status.isSuccess() then
        Future.successful(
          HttpResponse(
            status = StatusCodes.OK,
            entity = HttpEntity.Chunked.fromData(ContentTypes.`application/octet-stream`, response.entity.dataBytes)
          )
        )
      else
        response.entity.toStrict(2.seconds).map { strict =>
          HttpResponse(status = response.status, entity = strict.data.utf8String)
        }
    }
