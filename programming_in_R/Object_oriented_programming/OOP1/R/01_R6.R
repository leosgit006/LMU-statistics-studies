
# Write an R6-class `GameCounter` that counts points of players in a multiplayer
# game.
#
# The class should have an `$initialize` method taking one argument:
# - `players`: positive integer `numeric(1)` indicating the number of players.
#
# The class should have three methods:
# - `$point(player)`, where `player` is an integer `numeric(1)` indicating which
#   player gets a point. The function should throw an error if an invalid number
#   is given. The function should return `invisible(self)`, making it possible
#   to chain applications of `$point()` or `$reset()`.
# - `$winner()`, returning an integer `numeric(1)` indicating which player is
#   currently in the lead, or NA_real_ if no single player currently is leading
# - `$reset()`, resetting the game to the initial state, with all players having
#   0 points.  The function should return `invisible(self)`, making it possible
#   to chain applications of `$point()` or `$reset()`.
#
# There should be (at least) one public field `$score`, a `numeric`, indicating
# the points of each player.
#
# Example interactions with the class would be:
# > game.counter <- GameCounter$new(3)
# > game.counter$score  # initialized with 0
# #> [1] 0 0 0
# > game.counter$winner()  # NA_real_, printed as "NA"
# #> [1] NA
# > game.counter$point(2)
# > game.counter$point(1)
# > game.counter$point(1)
# > game.counter$score
# #> [1] 2 1 0
# > game.counter$winner()
# #> [1] 1                        # player 1 wins (with 2 points)
# > game.counter$reset()$point(2)
# > game.counter$winner()
# #> [1] 2                        # after the reset, player 2 has 1 point.
# > game.counter$point(3)$point(3)$winner()
# #> [1] 3
# > game.counter$score
# #> [1] 0 1 2

GameCounter <- R6Class("GameCounter",
  public = list(
    # write fields and methods here
    score = NULL,  ##done

    point = function(player) {
      assertIntegerish(player, len = 1)
      self$score[player] <- self$score[player] + 1
      invisible(self)
      ##done
    },

    winner = function() {
      if (sum(self$score == max(self$score)) > 1) {
        return(NA_real_)
      } else {
        return(which.max(self$score))  ##done
        }
    },

    reset = function() {
      self$score <- rep(0, length(self$score))
      invisible(self)
      ##done
    },

    initialize = function(players) {
      assertIntegerish(players, len = 1, )
      self$score <- rep(0, players)
      ##done
    }
  )
)
