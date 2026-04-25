

# The Game "Tic Tac Toe" is a two player game with players taking turns occupying spaces on a 3x3 grid.
# A player wins by occupying either a full diagonal, a row, or a column of 3 spaces.
#
# The playing field is a 3x3 matrix, with columns named A..C, and rows numbered 1..3
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 |   |   |   |
# --+---+---+---+
#
# We are considering a modified "Tic Tac Toe" game with "gravity": The players take turns choosing a
# column "A", "B" or "C". They then occupy the lowest empty field of that column, as if they had put
# a token into the column and it had fallen down. E.g.:
#
# Player "X" chooses "A":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 | X |   |   |
# --+---+---+---+
#
# Player "O" chooses "B":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 | X | O |   |
# --+---+---+---+
#
# Player "X" chooses "A":
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 | X |   |   |
# --+---+---+---+
# 3 | X | O |   |
# --+---+---+---+
#
# We are calling this version of the game "Gravity Tic Tac Toe"
#
# The goal of this exercise is to write a function that lets someone play 3x3 Gravity Tic Tac Toe against another
# human player, or against the computer.
# The state of the game (i.e. which fields are occupied by which players) is called the "position", similar to chess.
# Positions are represented by a 3x3-matrix with entries "X", "O" or NA.

# Part 01: Write a function that takes a position matrix and determines if there is a winner, and if
# so, who has won. The function should have a single input `position`; If this is not
# a 3x3 character matrix that may only contain "X", "O" or NA, an error should be thrown.
# The return value should be:
# - "X", if the "X"-player has won
# - "O", if the "O"-player has won
# - "" (empty string), if the game is a draw: no player has won, but there are no more possible moves
# - NA, if no player has won yet
# an error should be thrown if there are "two winners", e.g. a row of "X" and another row of "O",
# since this is an invalid position.
# It is not necessary to check for other indicators of invalid states. A position with two "X" and
# no "O" should explicitly be allowed, even though it would not be a reachable state in normal play
# (this could happen if one is giving one player an initial "advantage").
# Likewise, it is not necessary to verify whether `position` is a valid position when considering "gravity":
# Although a position where A3 is NA but A2 is "X" or "O" is not possible in this game, checking for this
# is not necessary.
#
# Hint: If you use `assertMatrix(mode = "character")`, you will find that
# `matrix(NA, nrow = 3, ncol = 3)` will trigger an error, because the "mode" of `NA` is `logical`
# by default. To get a character-NA-matrix, use `matrix(NA_character_, nrow = 3, ncol = 3)`. See
# > is.character(NA)  # FALSE
# > is.character(NA_character_)  # TRUE
# (Advanced students: There is a reason why the default `NA` is of type `logical`, can you think of what it is?)
ex01Winner <- function(position) {
  assertMatrix(position, mode = "character", ncols = 3, nrows = 3, any.missing = TRUE)
  if (!all(position %in% c("X", "O", NA_character_))) {
    stop("Invalid characters in position")
  }
  x.reih.win <- any(apply(position == "X" & !is.na(position), 1, all))
  x.spal.win <- any(apply(position == "X" & !is.na(position), 2, all))
  x.diag1.win <- all(diag(position) == "X" & !is.na(diag(position)))
  x.diag2.win <- all(diag(position[, 3:1]) == "X" & !is.na(diag(position[, 3:1])))
  x.won <- x.reih.win || x.spal.win || x.diag1.win || x.diag2.win
  o.reih.win <- any(apply(position == "O" & !is.na(position), 1, all))
  o.spal.win <- any(apply(position == "O" & !is.na(position), 2, all))
  o.diag1.win <- all(diag(position) == "O" & !is.na(diag(position)))
  o.diag2.win <- all(diag(position[, 3:1]) == "O" & !is.na(diag(position[, 3:1])))
  o.won <- o.reih.win || o.spal.win || o.diag1.win || o.diag2.win
  if (x.won && o.won) {
    stop("Two winners")
  }
  if (x.won) {
    return("X")
  }
  if (o.won) {
    return("O")
  }
  if (any(is.na(position))) {
    return(NA_character_)
  }
  return("")
}

# Part 02: Write a function that takes a Gravity Tic Tac Toe position and a column that a player chooses to
# play, and converts them to numeric c(row, col) matrix coordinates of where the player's token will come
# to rest, checking if the column is a valid move in the process.
#
# Your function should take two inputs: `column`, and `position`.
# - `column` must be checked to be a `character(1)` with no missing values (using checkmate).
# - `position` should be a 3x3 position matrix with same constraints as in ex01Winner.
#   (It is *not* necessary to check whether `position` has a winner, or if there are two winners etc.,
#   but it should be checked for being a character matrix with valid content)
# `column` must be one of "A", "B", or "C". If it is not one of these, or if the indicated column is already
# full (i.e. the first row of that column is occupied), an error of class
# "InvalidMoveError" should be thrown, for example by using
# `stop(errorCondition(<message>, class = "InvalidMoveError"))`
# Note the difference between having an invalid type for `column` (e.g. if it is not a `character`, or if it
# is a `character` vector with more than one element), and having `column` of correct type (`character(1)`) but
# with invalid *content*. The first should be checked with `checkmate`, the second should generate an
# `"InvalidMoveError"`.
# You may assume without checking that `position` is a valid matrix when considering "gravity", i.e. that if
# row r, col c are occupied by "X" or "O", then all rows below r of that column c are also occupied.
# An example `position` could be
example.position <- matrix(
    c("X",  NA,  NA,
      "O", "X",  NA,
      "X", "O",  NA),
  ncol = 3, byrow = TRUE)
# In this case, with `column` "A", the function should throw an `InvalidMoveError`. With `column` "B", the
# function should return c(1, 2). With `column` "C", the return value should be c(3, 3).
ex02MoveStringToCoordinate <- function(column, position) {
  # checkmate-Prüfungen
  assertString(column, na.ok = FALSE)
  assertMatrix(position, mode = "character", ncols = 3, nrows = 3, any.missing = TRUE)
  if (!all(position %in% c("X", "O", NA_character_))) {
    stop("Invalid characters in position")
  }

  col_map <- c("A" = 1, "B" = 2, "C" = 3)
  col_upper <- toupper(column)

  if (!col_upper %in% names(col_map)) {
    stop(errorCondition(
      paste0("Invalid column: ", col_upper, ". Must be 'A', 'B', or 'C'."),
      class = "InvalidMoveError"
    ))
  }

  col_idx <- col_map[[col_upper]]

  # Oberste Zeile besetzt → Spalte voll
  if (!is.na(position[1, col_idx])) {
    stop(errorCondition(
      paste0("Column ", col_upper, " already full."),
      class = "InvalidMoveError"
    ))
  }
  # Niedrigste freie Zeile finden (von unten nach oben = Zeile 3 bis 1)
  row_idx <- max(which(is.na(position[, col_idx])))
  c(row_idx, col_idx)
}

# Part 03: Write a function that interacts with a human player. Your function should take the arguments
# `position` and `player`.
# - `position` should be a 3x3 position matrix, checked in the same way as in ex02MoveStringToCoordinate.
# - `player` should be checked to be either the string "X" or the string "O".
#
# The function is called whenever the current position is `position`, and player `player` is asked to
# make a move. If the game is already over (a player has won, or there is a draw, as per ex01Winner),
# then an error should be thrown, as the player can no longer make a move.
# Otherwise, the function should prompt the player to make his choice, e.g. by printing the position,
# announcing that player `player` is to play, and getting an input by using the `readline()` function.
# (It is essential that `readline()` is called to prompt for input; this function will be replaced
# in the submission check).
# The user input should be sanitized (removing spaces e.g. using `gsub` or `trimws`, converting input
# to uppercase using `toupper`, so that an input of "  a " still counts as valid) and checked:
#  - if the input is "Q", an error should be thrown because the player has ended the game.
#  - if the input is not a valid coordinate as by `ex02MoveStringToCoordinate`, e.g. because the chosen
#    space is occupied or the input is not valid, then an information message should be printed
#    and the user should be prompted *again* for a new input. It is recommended to use the
#    ex02MoveStringToCoordinate function here, in combination with tryCatch, to check user input.
#
# The function should return the numeric coordinate c(row, col) where the token of the player will be
# placed; this is the result of ex02MoveStringToCoordinate of the user's input if the input was valid.
#
# Example interaction with the program:
# > ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 "O" "O" NA
## 3 "X" "O" NA
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> d
## Invalid move: D.
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> a
## Column A already full.
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> b
# <RETURN VALUE:> c(1, 2)
#
# > ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 "O" "O" NA
## 3 "X" "O" NA
## Player X to move. What is your move?
## ("A", "B", "C"; or "Q" to quit): <INPUT> Q
## Error in ex03HumanPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA),  :
##   Player X Quit
#
# The output of your answer does not need to be exactly like this, and
# the tests do not check if the function prints anything, but it is recommended to give some output
# to make everything nicer.
ex03HumanPlayer <- function(position, player) {
  assertMatrix(position, mode = "character", ncols = 3, nrows = 3, any.missing = TRUE)
  if (!all(position %in% c("X", "O", NA_character_))) stop("Invalid characters in position")
  assertChoice(player, c("X", "O"))

  winner <- ex01Winner(position)
  if (!is.na(winner)) stop("Game is already over.")

  # Position anzeigen
  colnames(position) <- c("A", "B", "C")
  rownames(position) <- 1:3
  message("Position:")
  print(position)

  repeat {
    message(sprintf(
      'Player %s to move. What is your move?\n("A", "B", "C"; or "Q" to quit): ',
      player
    ))
    input <- readline()
    input <- toupper(trimws(input))

    if (input == "Q") {
      stop(paste("Player", player, "Quit"))
    }

    coord <- tryCatch(
      ex02MoveStringToCoordinate(input, position),
      InvalidMoveError = function(e) {
        message("Invalid move: ", conditionMessage(e))
        NULL
      },
      error = function(e) {
        message("Invalid input: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(coord)) return(coord)
  }
}


# Part 04: Write a function that makes random, but valid, Gravity Tic Tac Toe moves
#
# The function should have the same arguments as the human player: `position`, and `player`,
# and they should be checked the same way (validity, and whether the game is already over).
# The function should return a numeric coordinate c(row, col) where the player's token is
# placed, which must be the result of a a valid move.
#
# > ex04RandomPlayer(matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3), "X")
# --> c(1, 2) or c(3, 3)
#
# Regardless of the name of the function, the output does not *need* to be randomised, it should
# just be *something* that is not further checked by the submission script besides being a legal move.
#
# Note how both ex03HumanPlayer and ex04RandomPlayer work the same way: they have the same
# inputs and outputs, only one of them works internally while the other asks something of the user.
ex04RandomPlayer <- function(position, player) {
  assertMatrix(position, mode = "character", ncols = 3, nrows = 3, any.missing = TRUE)
  if (!all(position %in% c("X", "O", NA_character_))) stop("Invalid characters in position")
  assertChoice(player, c("X", "O"))

  winner <- ex01Winner(position)
  if (!is.na(winner)) stop("Game is already over.")

  valid_cols <- c("A", "B", "C")[which(is.na(position[1, ]))]

  # Zufällige gültige Spalte wählen
  chosen <- sample(valid_cols, 1)
  ex02MoveStringToCoordinate(chosen, position)
}

# Write a function that lets two human players play against each other, or one player play
# against a computer opponent making random moves.
#
# Your function should have four arguments: `playerX`, `playerO`, `starting.player`, and `starting.position`.
# - `playerX` should be a *function*. It could, for example, be ex03HumanPlayer, or ex04RandomPlayer.
# - `playerO` should be a function as well, just as playerX.
# - `starting.player` should be a string and must be either "X" or "O". This argument should be optional,
#    defaulting to "X".
# - `starting.position` should be a valid position matrix, as already seen in ex02MoveStringToCoordinate.
#    This argument should be optional, defaulting to `matrix(NA_character_, 3, 3)`.
#
# This function is supposed to host a Gravity Tic Tac Toe game and should operate in a loop.
# 1. It should be checked whether the game is over. If so, the identifier of the winning player should be returned,
#    "X", "O", or "" (draw). It is recommended to use ex01Winner for this.
# 2. The next "player" should be asked for his move. Call the playerX or playerO function with the current
#    position (this may be a human player or the random player); the return value of the function is a
#    numeric c(row, col) coordinate where the player's token should be placed.
# 3. Do not trust the return value of the `"player"` function and check that the move made by the player is valid;
#    throw an error if not. You should use checkmate, and also check by verifying that ex02MoveStringToCoordinate
#    applied to the `col` (when converted to a letter) returns the same c(row, col).
# 4. update the position matrix by inserting an "X" or an "O" at the correct space.
#
# If the playerX / playerO functions don't return invalid moves, this loop should eventually end and
# return a "X", "O" or "" string. In fact, if the starting position is a win for a player (or a draw), this
# function should return immediately without calling a player function.
#
# It may be useful to have this function generate some output. However,
# if you choose to generate output, you *MUST* use `message()` to communicate with the user. Don't use print(), cat(),
# dput() or similar.
#
# Example calls:
# > ex05Tournament(ex03HumanPlayer, ex04RandomPlayer)
#  (human player playing against random, the "X" player begins on an empty field)
# > ex05Tournament(ex03HumanPlayer, ex03HumanPlayer, "O")
#  (two humans playing against each other, the "O" player starting)
# > ex05Tournament(ex04RandomPlayer, ex04RandomPlayer,
#     starting.position = matrix(c("X", "O", "X", NA, "O", "O", NA, NA, NA), nrow = 3))
#  (two random players on a partially occupied field; enjoy the show.)
ex05Tournament <- function(playerX, playerO,
                           starting.player   = "X",
                           starting.position = matrix(NA_character_, 3, 3)) {
  assertFunction(playerX)
  assertFunction(playerO)
  assertChoice(starting.player, c("X", "O"))
  assertMatrix(starting.position, mode = "character", ncols = 3, nrows = 3, any.missing = TRUE)
  if (!all(starting.position %in% c("X", "O", NA_character_))) {
    stop("Invalid characters in starting.position")
  }

  col_letter <- c("A", "B", "C")
  position   <- starting.position
  current    <- starting.player

  repeat {
    # 1. Spielende prüfen
    result <- ex01Winner(position)
    if (!is.na(result)) {
      if (result == "") {
        message("Draw!")
      } else {
        message(sprintf("Player %s wins!", result))
      }
      return(result)
    }

    # 2. Spieler um Zug bitten
    player_fn <- if (current == "X") playerX else playerO
    coord <- player_fn(position, current)
    
    # 3. Rückgabewert validieren
    assertIntegerish(coord, len = 2, lower = 1, upper = 3, any.missing = FALSE)
    coord <- as.integer(coord)
    col_idx <- coord[2]
    expected <- ex02MoveStringToCoordinate(col_letter[col_idx], position)
    if (!identical(coord, expected)) {
      stop(sprintf("Player %s returned an invalid move: (%d, %d)", current, coord[1], coord[2]))
    }

    # 4. Position aktualisieren
    position[coord[1], coord[2]] <- current
    message(sprintf("Player %s played column %s -> (%d, %d)",
                    current, col_letter[col_idx], coord[1], coord[2]))

    # Spieler wechseln
    current <- if (current == "X") "O" else "X"
  }
}
