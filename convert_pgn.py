import chess.pgn
import sys
import argparse

COMPLETE_RESULTS = {"1-0", "0-1", "1/2-1/2"}

def convert_pgn(input_file, output_file, max_games: int, max_moves: int):
    print(f"Converting '{input_file}' → '{output_file}'")
    print(f"Limit: {max_games} games, {max_moves} half-moves per game")
    count = 0
    skipped = 0
    with open(input_file, "r", encoding="utf-8", errors="replace") as in_pgn:
        with open(output_file, "w", encoding="utf-8") as out_pgn:
            while count < max_games:
                game = chess.pgn.read_game(in_pgn)
                if game is None:
                    break

                result = game.headers.get("Result", "*")
                # Skip games without a decisive/drawn result (e.g. aborted games)
                if result not in COMPLETE_RESULTS:
                    skipped += 1
                    continue

                # Write minimal tags (Event + Result is enough for PgnOpeningDatabase)
                out_pgn.write(f'[Event "{game.headers.get("Event", "?")}"]\n')
                out_pgn.write(f'[Result "{result}"]\n\n')

                # Write moves in coordinate (UCI) format
                board = game.board()
                moves_str = []
                for i, move in enumerate(game.mainline_moves()):
                    if i % 2 == 0:
                        moves_str.append(f"{(i // 2) + 1}.")
                    moves_str.append(move.uci())
                    board.push(move)
                    if i >= max_moves - 1:
                        break

                moves_str.append(result)
                out_pgn.write(" ".join(moves_str) + "\n\n")

                count += 1
                if count % 10000 == 0:
                    print(f"  Processed {count:,} games (skipped {skipped:,})...")

    print(f"Done! Converted {count:,} games, skipped {skipped:,} incomplete games.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Convert PGN (SAN) to coordinate-notation PGN for TiChess.")
    parser.add_argument("input",  help="Input PGN file (SAN notation)")
    parser.add_argument("output", help="Output PGN file (UCI coordinate notation)")
    parser.add_argument("--max-games", type=int, default=100_000,
                        help="Maximum number of games to convert (default: 100000)")
    parser.add_argument("--max-moves", type=int, default=30,
                        help="Maximum half-moves per game to include (default: 30 = 15 full moves)")
    args = parser.parse_args()
    convert_pgn(args.input, args.output, args.max_games, args.max_moves)
