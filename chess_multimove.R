library(chess)

# Initialize a chess game
start_game <- function() {
  game <- chess()
  print("♟️ Starting Chess.Multimove_R!")
  return(game)
}

# Display the current board state
display_board <- function(game) {
  print(game)
  cat("\nFEN:", export(game, format = "fen"), "\n")
}

# Handle a single move
player_move <- function(game, move) {
  if (is_move_legal(game, move)) {
    game <- move(game, move)
    print(paste("✅ Player Move:", move))
  } else {
    print(paste("🚨 Illegal move:", move))
  }
  return(game)
}

# Simulate AI move (random legal move)
ai_move <- function(game) {
  legal_moves <- legal_moves(game)
  if (length(legal_moves) > 0) {
    move <- sample(legal_moves, 1)  # Pick a random legal move
    game <- move(game, move)
    print(paste("🤖 AI Move:", move))
  } else {
    print("🚨 No legal moves available for AI.")
  }
  return(game)
}

# Play the game with multiple moves
play_game <- function() {
  game <- start_game()
  
  while (!is_game_over(game)) {
    display_board(game)
    
    # Get player move
    player_input <- readline(prompt = "Your move (e.g., e2e4): ")
    if (tolower(player_input) == "quit") {
      print("👋 Game ended by player.")
      break
    }
    
    # Execute player's move
    game <- player_move(game, player_input)
    if (is_game_over(game)) {
      break
    }
    
    # AI responds
    game <- ai_move(game)
  }
  
  display_board(game)
  print("🏁 Game Over!")
}

# Run the game
play_game()
