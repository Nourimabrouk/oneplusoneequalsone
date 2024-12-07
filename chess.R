import chess
import chess.engine
import time

class ChessR:
  def __init__(self):
  self.board = chess.Board()
self.engine_path = "stockfish"  # Ensure Stockfish engine is installed and in PATH
self.engine = None

def start_engine(self):
  try:
  self.engine = chess.engine.SimpleEngine.popen_uci(self.engine_path)
print("🔥 Chess engine initialized. Let's GO!")
except Exception as e:
  print("🚨 Engine failed to start:", e)

def display_board(self):
  print(self.board)
print("\nFEN:", self.board.fen())

def player_move(self, move_uci):
  try:
  move = chess.Move.from_uci(move_uci)
if move in self.board.legal_moves:
  self.board.push(move)
print(f"✅ Player Move: {move_uci}")
else:
  print("🚨 Illegal move. Try again.")
except ValueError:
  print("🚨 Invalid move format. Use UCI notation (e.g., e2e4).")

def engine_move(self, time_limit=1.0):
  if self.engine:
  result = self.engine.play(self.board, chess.engine.Limit(time=time_limit))
self.board.push(result.move)
print(f"🤖 Engine Move: {result.move}")
else:
  print("🚨 Engine is not running. Start it first!")

def play_game(self):
  print("♟️ Starting a new game of Chess.R! Make your moves in UCI format (e.g., e2e4).")
while not self.board.is_game_over():
  self.display_board()
player_input = input("Your move: ")
if player_input.lower() == "quit":
  print("👋 Game ended by player.")
break
self.player_move(player_input)
if self.board.is_game_over():
  break
self.engine_move()
self.display_board()
print("🏁 Game Over:", self.board.result())

def quit_engine(self):
  if self.engine:
  self.engine.quit()
print("🔌 Chess engine closed. GG!")

if __name__ == "__main__":
  # Initialize Chess.R
  game = ChessR()
game.start_engine()

try:
  # Play the game
  game.play_game()
except KeyboardInterrupt:
  print("\n👋 Game interrupted by player.")
finally:
  # Close the engine on exit
  game.quit_engine()
