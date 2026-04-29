import chess.pgn
import collections
import math

def analyze(pgn_path):
    print(f"Analyzing {pgn_path}...")
    
    # Position (FEN) -> Move -> {wins, draws, losses, total}
    db = collections.defaultdict(lambda: collections.defaultdict(lambda: {"w": 0, "d": 0, "l": 0, "n": 0}))
    
    with open(pgn_path) as f:
        count = 0
        while count < 100000: # Analyze a good chunk of the file
            game = chess.pgn.read_game(f)
            if game is None: break
            
            result = game.headers.get("Result")
            if result not in ["1-0", "0-1", "1/2-1/2"]: continue
            
            board = game.board()
            for move in game.mainline_moves():
                # We use a simplified FEN (no move counters) to group positions
                fen = board.fen().split(" ")[0] + " " + board.fen().split(" ")[1]
                
                m_str = move.uci()
                stats = db[fen][m_str]
                stats["n"] += 1
                if result == "1-0":
                    if board.turn == chess.WHITE: stats["w"] += 1
                    else: stats["l"] += 1
                elif result == "0-1":
                    if board.turn == chess.BLACK: stats["w"] += 1
                    else: stats["l"] += 1
                else:
                    stats["d"] += 1
                
                board.push(move)
                if board.fullmove_number > 15: break
            
            count += 1
            if count % 20000 == 0: print(f"  Read {count} games...")

    # Statistics on frequencies
    freqs = []
    for fen in db:
        for m in db[fen]:
            freqs.append(db[fen][m]["n"])
    
    freqs.sort()
    print("\n--- Distribution of Move Frequencies ---")
    if freqs:
        print(f"Total unique position-move pairs: {len(freqs)}")
        print(f"Max frequency: {freqs[-1]}")
        print(f"90th percentile: {freqs[int(len(freqs)*0.9)]}")
        print(f"95th percentile: {freqs[int(len(freqs)*0.95)]}")
        print(f"99th percentile: {freqs[int(len(freqs)*0.99)]}")
        
        # Check for "High Frequency" competition
        print("\n--- Competition Analysis (Positions with multiple high-freq moves) ---")
        comp_count = 0
        for fen in db:
            high_freq_moves = [m for m, s in db[fen].items() if s["n"] > 50]
            if len(high_freq_moves) > 1:
                comp_count += 1
        print(f"Positions with >1 move having >50 games: {comp_count}")

if __name__ == "__main__":
    # Note: We analyze the ORIGINAL PGN if possible, or the converted one.
    # Since openings.pgn is converted to UCI, we can use it.
    analyze("src/main/resources/openings.pgn")
