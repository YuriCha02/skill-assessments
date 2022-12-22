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

#This is player's movement
player_move <- function() {
  row0.5 <- "stdin"
  cat("\nChoose the row: ")
  row <- readLines(con = row0.5, n = 1)
  row_int <- as.integer(row)
  col0.5 <- "stdin"
  cat("Choose the col: ")
  col <- readLines(con = col0.5, n = 1)
  col_int <- as.integer(col)
  player_mark <- environment[row_int, col_int]
  if(!is.null(player_mark) && !is.na(player_mark)) {
    if(player_mark != "X" & player_mark != "O") {
    environment[row_int, col_int] <<- player_shape
    cat("\n////////////////\n")
    print(environment)
    cat("////////////////\n")
    turn <<- turn + 1
    try(check_winner())
    computer_move()
    } else {
      cat("\nIllegal movement!\nDo it again \n")
      player_move()
    }
  } else {
    cat("\nIllegal movement!\nDo it again \n")
    player_move()
  } 
}

# This is computer's movement
computer_move <- function() {
  while (is.null(winner)) {
    cat("\n")
    cat("Computer's movement \n")
    # Find unoccupied spot for computer
    empty_spot <<- which(environment != "X" & environment != "O")
    
    # replace the empty spot
    environment[environment == sample(empty_spot, 1)] <<- computer_shape
    cat("\n////////////////\n")
    print(environment)
    cat("////////////////\n")
    turn <<- turn + 1
    try(check_winner())
    player_move()
  }
}

#Ask if player wants to go first or second
Start_game <- function() {
  if (interactive()) {
    con <- stdin()
  } else {
    con <- "stdin"
  }
  #Ask if player wants to go first or second
  cat("Would you like to go first or second? \nType 1 if you like to go first or type 2 if you prefer to go second.\n")
  firstorsecond <- readLines(con = con, n = 1)
  if (firstorsecond == 1) {
    player_shape <<- "X"
    computer_shape <<- "O"
    cat("\nYou're going first \nYou're shape is X")
    cat("\n////////////////\n")
    print(environment)
    cat("////////////////\n")
    player_move()
  } else if (firstorsecond == 2) {
    player_shape <<- "O"
    computer_shape <<- "X"
    cat("\nYou're going second \nYou're shape is O\n")
    print(environment)
    computer_move()
  } else {
    cat("Can't recognize your input.\nType 1 or 2\n")
    cat("///////////////////////////////////////\n")
    Start_game()
  }
}

#Start Game
Start_game()
