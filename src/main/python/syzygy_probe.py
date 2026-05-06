#!/usr/bin/env python3
import sys

import chess
import chess.syzygy


def move_score(board, tablebase, move):
    board.push(move)
    try:
        wdl_after = tablebase.probe_wdl(board)
        dtz_after = tablebase.probe_dtz(board)
    finally:
        board.pop()

    our_wdl = -wdl_after
    our_dtz = -dtz_after

    if our_wdl > 0:
        dtz_score = -abs(our_dtz)
    elif our_wdl == 0:
        dtz_score = -abs(our_dtz)
    else:
        dtz_score = abs(our_dtz)

    return (our_wdl, dtz_score)


def main():
    if len(sys.argv) < 3:
        print("usage: syzygy_probe.py <tablebase-path> <fen>", file=sys.stderr)
        return 2

    tablebase_path = sys.argv[1]
    fen = " ".join(sys.argv[2:])
    board = chess.Board(fen)
    if not board.is_valid():
        print("invalid board", file=sys.stderr)
        return 1

    try:
        with chess.syzygy.open_tablebase(tablebase_path) as tablebase:
            legal_moves = list(board.legal_moves)
            if not legal_moves:
                return 1

            best_move = max(legal_moves, key=lambda move: move_score(board, tablebase, move))
            root_wdl = tablebase.probe_wdl(board)
            root_dtz = tablebase.probe_dtz(board)
    except chess.syzygy.MissingTableError as error:
        print(str(error), file=sys.stderr)
        return 1

    print(f"bestmove {best_move.uci()}")
    print(f"wdl {root_wdl}")
    print(f"dtz {root_dtz}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
