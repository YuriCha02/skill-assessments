# This is the board.
environment <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
colnames(environment) <- c("[1]", "[2]", "[3]")
row.names(environment) <- c("[1]", "[2]", "[3]")



# This is defualt turn
turn <- 0

# This will evaluate if the victory condition is met except for tie condition.
check_winner <- function(X) {
  environment <- as.matrix(environment)
  
  # This is default value for the winner
  winner <- 0
  # Check rows
  for (i in 1:nrow(environment)) {
    if (all(environment[i, ] == X)) {
      winner <- 1
    }
  }
  
  # Check columns
  for (i in 1:ncol(environment)) {
    if (all(environment[, i] == X)) {
      winner <- 1
    }
  }
  
  # Check diagonals
  if (all(diag(environment) == X)) {
    winner <- 1
  }
  
  if (environment[3, 1] == X & environment[2, 2] == X & environment[1, 3] == X) {
    winner <- 1
  }
  
  else if (turn == 9 & winner == 0) {
    winner <- 2
  }
  
  return(winner)
}

# This will display environment in formatted way
display <- function(environment) {
  cat("\n////////////////\n")
  print(environment)
  cat("////////////////\n")
}

# This will stop the script without producing error
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

# This is player's movement
player_move <- function(player_shape, computer_shape) {
    row0.5 <- "stdin"
    cat("\nChoose the row: ")
    row <- readLines(con = row0.5, n = 1)
    row_int <- as.integer(row)
    col0.5 <- "stdin"
    cat("Choose the col: ")
    col <- readLines(con = col0.5, n = 1)
    col_int <- as.integer(col)
    player_mark <- environment[row_int, col_int]
    if (!is.null(player_mark) && !is.na(player_mark)) {
      if (player_mark != "X" & player_mark != "O") {
        environment[row_int, col_int] <<- player_shape
        turn <<- turn + 1
        display(environment)
        if (check_winner(player_shape) == 1) {
          cat("You won!\n")
          stop_quietly()
        } else if (check_winner(player_shape) == 2) {
          cat("Tie\n")
          stop_quietly()
        } else {
          computer_move(player_shape, computer_shape)
        }
      } else {
        cat("\nIllegal movement!\nDo it again \n")
        player_move(player_shape, computer_shape)
      }
    } else {
      cat("\nIllegal movement!\nDo it again \n")
      player_move(player_shape, computer_shape)
    }
}

# This is computer's movement
computer_move <- function(player_shape, computer_shape) {
    cat("\n")
    cat("Computer's move \n")
    # Find unoccupied spot for computer
    empty_spot <- which(environment != "X" & environment != "O")
    # replace the empty spot
    if (length(empty_spot) != 1) {
      environment[environment == sample(empty_spot, 1)] <<- computer_shape
      turn <<- turn + 1
    } else {
      environment[environment == empty_spot] <- computer_shape
      turn <<- turn + 1
    }
    display(environment)
    if (check_winner(computer_shape) == 1) {
      cat("You lost\n")
      stop_quietly()
    } else if (check_winner(computer_shape) == 2) {
      cat("Tie\n")
      stop_quietly()
    } else {
      player_move(player_shape, computer_shape)
    }
}

# Ask if player wants to go first or second
Start_game <- function() {
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  # Ask if player wants to go first or second
  cat("Would you like to go first or second? \nType 1 if you like to go first or type 2 if you prefer to go second.\n")
  firstorsecond <- readLines(con = con, n = 1)
  if (firstorsecond == 1) {
    player_shape <- "X"
    computer_shape <- "O"
    cat("\nYou're going first \nYou're shape is X\n")
    display(environment)
    player_move(player_shape, computer_shape)
  } else if (firstorsecond == 2) {
    player_shape <<- "O"
    computer_shape <<- "X"
    cat("\nYou're going second \nYou're shape is O\n")
    display(environment)
    computer_move(player_shape, computer_shape)
  } else {
    cat("Can't recognize your input.\nType 1 or 2\n")
    cat("///////////////////////////////////////\n")
    Start_game()
  }
}

# Start Game
Start_game()
