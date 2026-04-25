# In this exercise, you will be trying to make functions faster.
#
# Although performance is almost always decidedly less important than
# correctness and legibility, it is still helpful to know how fast/slow
# operations are in R.
#
# In the following exercises, you are given a reference-solution for each
# exercise. Your task is to write a function that behaves the same but runs
# in less than 1/2 the time for the specified input (median speedup,
# as per `microbenchmark()`).
#
# Feel free to go for larger speed-ups: Many of the functions can easily be
# sped up much more than by a factor of 2, and experimenting with different
# approaches will give you valuable experience.
#
# Note: Because the reference functions were written to be deliberately slow,
# they should not serve as inspiration for your own code in the future!

# You have to make a decision between two alternative choices, choice A and
# choice B. To help in your decision, you have asked a large panel of experts
# in different fields how much they think choice A and choice B are preferrable.
# The experts evaluated the choices based on different things they consider
# relevant, such as profitability, environmental sustainability,
# effect on public relations, etc., and each expert has given choice A and
# choice B a score between 0 and 1. You will probably consider the experts'
# judgement in more detail, because some experts may be more reliable than
# others or consider more important issues than others. However, there is a
# specific shortcut you can take: If *no* expert has given choice A a *lower*
# score than choice B, but *at least one* expert has given choice A a *higher*
# score than choice B, then choice A "dominates" choice B, and you can disregard
# choice B right away.
# This is the concept of "Pareto Dominance" or "Pareto Efficiency":
# <https://en.wikipedia.org/wiki/Pareto_efficiency>.
#
# Write a function that calculates whether choice A dominates choice B that
# is faster than the following reference implementation.
# Inputs:
# - `scores.a`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice A.
# - `scores.b`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice B.
# `scores.a` and `scores.b` should have the same length.
# The `i`th component of `scores.a` and `scores.b` are the scores given by
# expert `i`, so they can be compared directly.
#
# Your function should have a median runtime 1/2 as much as
# `ex01DominatesReference` on input for `scores.a` and `scores.b` generated
# as follows:
# scores.a <- runif(400, 0.478, 1)
# scores.b <- runif(400, 0, 0.522)
# (I.e. evaluations from 400 experts, choice A dominates choice B in
# approximately 50% of cases)
# However, the correctness of your function is also checked for other input.
ex01DominatesReference <- function(scores.a, scores.b) {
  assertNumeric(scores.a, any.missing = FALSE, lower = 0, upper = 1)
  assertNumeric(scores.b, any.missing = FALSE, lower = 0, upper = 1, len = length(scores.a))

  a.can.dominate <- FALSE
  a.can.not.dominate <- FALSE
  for (i in seq_along(scores.a)) {
    if (scores.a[[i]] < scores.b[[i]]) a.can.not.dominate <- TRUE
    if (scores.a[[i]] > scores.b[[i]]) a.can.dominate <- TRUE
  }
  if (a.can.not.dominate) return(FALSE)
  if (a.can.dominate) return(TRUE)
  FALSE
}

