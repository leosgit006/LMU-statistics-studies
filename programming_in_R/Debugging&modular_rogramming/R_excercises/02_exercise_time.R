
# Write a function that measures how long another function was running. The
# function should return the runtime of the given function, in seconds, rounded
# to the nearest integer second.
#
# The function being timed may sometimes throw an error, in which case the
# return value should depend on the `impute.inf`-argument: If it is `TRUE`, a
# value of `Inf` should be returned when an error was thrown, instead of the
# actual runtime. This would indicate that the function never actually finished.
#
# This function gets three arguments:
# - `fn` (function) the function to measure. It takes no arguments and should
#   be called as `fn()`.
# - `impute.inf` (`logical(1)`) whether to return `Inf`, instead of the runtime,
#   when `fn` throws an error.
#
# Functions that may be useful here are the `?system.time()` function, or the
# `?proc.time()` function. Make sure to use the `elapsed` part of the times that
# they report.
#
# Example functions to try out:
sleep1 <- function() Sys.sleep(1)
sleep2 <- function() Sys.sleep(2)
sleepErr <- function() {
  Sys.sleep(1)
  stop("error :-(")
}
# Example calls:
# > ex01TimeFun(sleep1, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleep1, impute.inf = TRUE)  # 1
# > ex01TimeFun(sleep2, impute.inf = FALSE)  # 2
# > ex01TimeFun(sleepErr, impute.inf = FALSE)  # 1
# > ex01TimeFun(sleepErr, impute.inf = TRUE)  # Inf
ex01TimeFun <- function(fn, impute.inf) {
  # your code
  assertFunction(fn)
  assertLogical(impute.inf, max.len = 1, any.missing = FALSE)

  time <- system.time({
    result <- try(fn(), silent = TRUE)
    })
  if (inherits(result, "try-error") && impute.inf) {
    return(Inf)
  } else {
    round(time[["elapsed"]])
  }
}
