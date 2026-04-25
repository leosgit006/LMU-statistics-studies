
# Write a function that counts how often the RNG was invoked by another
# function.
#
# Input:
#  - `func` (`function`): A function that takes no arguments and returns nothing.
#   This function will call `runif()` a varying number of times, which your own
#   function should count.
# Return value: An integer `numeric(1)`, indicating how often the RNG was
# invoked by `func`. The return value should be capped at 1000, i.e. if `func()`
# generates more than 1000 random numbers, then your function should return
# 1000. Do this to avoid infinite loops.
#
# Your function can find out how often `func()` called `runif()` by saving the
# RNG state before and after `func()` is called, then resetting the RNG state to
# what it was in the beginning and counting how often `runif()` needs to be
# called to get back to the end state.
#
# Note that `.Random.seed` is a *global* variable, so you will need to use the
# `<<-` operator to assign to it.
#
# Your function should not set a seed or otherwise change the RNG state before
# `func()` is called, and `func()` should be called only once. However, you can
# rely on the fact that `.Random.seed` is set when your function is called.
#
# Example usage:
# > ex01CountRng(function() { runif(1); runif(1) })  # returns 2
# > ex01CountRng(function() { runif(3) })  # returns 3
# > ex01CountRng(function() {})  # returns 0
# > ex01CountRng(function() { runif(2000) })  # returns 1000
ex01CountRng <- function(func) {
  # your code
  assertFunction(func)

## Storing the state before and after function was called
  state.before <- .Random.seed
  func()
  state.after <- .Random.seed

  .Random.seed <<- state.before
## Setting RNG
  counter <- 0
  while (!identical(.Random.seed, state.after) && counter < 1000) {
    runif(1)
    counter <- counter + 1
  }
  return(counter)
}
