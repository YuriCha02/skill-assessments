# This is the board.
environment <- data.frame(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9))
colnames(environment) <- c("[1]", "[2]", "[3]")
row.names(environment) <- c("[1]", "[2]", "[3]")

# This will set winner to its default value which is NULL
winner <- NULL

# This will evaluate if the victory condition is met
check_winner <- function() {
  if (player_shape == environment[1, 1] & player_shape == environment[1, 2] & player_shape == environment[1, 3]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[1, 1] & computer_shape == environment[1, 2] & computer_shape == environment[1, 3]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (player_shape == environment[2, 1] & player_shape == environment[2, 2] & player_shape == environment[2, 3]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[2, 1] & computer_shape == environment[2, 2] & computer_shape == environment[2, 3]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (player_shape == environment[3, 1] & player_shape == environment[3, 2] & player_shape == environment[3, 3]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[3, 1] & computer_shape == environment[3, 2] & computer_shape == environment[3, 3]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (player_shape == environment[1, 1] & player_shape == environment[2, 2] & player_shape == environment[3, 3]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[1, 1] & computer_shape == environment[2, 2] & computer_shape == environment[3, 3]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (player_shape == environment[3, 1] & player_shape == environment[2, 2] & player_shape == environment[1, 3]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[3, 1] & computer_shape == environment[2, 2] & computer_shape == environment[1, 3]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (player_shape == environment[1, 1] & player_shape == environment[2, 1] & player_shape == environment[3, 1]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[1, 1] & computer_shape == environment[2, 1] & computer_shape == environment[3, 1]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (player_shape == environment[1, 2] & player_shape == environment[2, 2] & player_shape == environment[3, 2]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[1, 2] & computer_shape == environment[2, 2] & computer_shape == environment[3, 2]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (player_shape == environment[1, 3] & player_shape == environment[2, 3] & player_shape == environment[3, 3]) {
    winner <<- "player"
    cat("You won!")
  } else if (computer_shape == environment[1, 3] & computer_shape == environment[2, 3] & computer_shape == environment[3, 3]) {
    winner <<- "computer"
    cat("You lost!")
  } else if (turn == 9) {
    cat("tie")
  } else {
    winner <<- NULL
  }
}

# This will used to show how many turn has passed.
turn <- 0

# This is the player's movement
player_movement <- function() {
  while (is.null(winner)) {
    # Acquiring where the player wants to move.
    cat("\nPlayer's turn\n")
    player_select_row <- readline("Choose the row: ")
    player_select_row_int <- as.integer(substr(player_select_row, 1, 1))
    player_select_col <- readline("choose the column: ")
    player_select_col_int <- as.integer(substr(player_select_col, 1, 1))
    player_mark <<- environment[player_select_row_int, player_select_col_int]

    # Checking if the next spot is already marked by player or computer
    if (!is.null(player_mark) && !is.na(player_mark)) {
      if (player_mark != "X" & player_mark != "O") {
        # If the spot isn't taken yet, replace the spot
        environment[player_select_row_int, player_select_col_int] <<- player_shape

        # Display the change and let computer make movement
        print(environment)
        turn <<- turn + 1
        try(check_winner())
        computer_movement()
      } else {
        cat("\nIllegal movement!\nDo it again \n")
        player_movement()
      }
    } else {
      cat("\nIllegal movement!\nDo it again \n")
      player_movement()
    }
  }
}

# This is computer's movement
computer_movement <- function() {
  while (is.null(winner)) {
    cat("\nComputer's movement \n")
    # Find unoccupied spot for computer
    empty_spot <<- which(environment != "X" & environment != "O")

    # replace the empty spot
    environment[environment == sample(empty_spot, 1)] <<- computer_shape
    print(environment)
    turn <<- turn + 1
    try(check_winner())
    player_movement()
  }
}

# This will change the player's shape depending on who's going first.
firstorsecond <- function() {
  Answer <- readline("Would you like to go first or second? \nType 1 if you like to go first or type 2 if you prefer to go second.\n")
  if (substr(Answer, 1, 1) == "1") {
    player_shape <<- "X"
    computer_shape <<- "O"
    cat("You're going first \nYou're shape is X\n")
    print(environment)
    player_movement()
  } else if (substr(Answer, 1, 1) == "2") {
    player_shape <<- "O"
    computer_shape <<- "X"
    cat("You're going second \nYou're shape is O\n")
    print(environment)
    computer_movement()
  } else {
    NULL
    cat("Do it again\n")
    firstorsecond()
  }
}

# Start the game
if (interactive()) firstorsecond()
