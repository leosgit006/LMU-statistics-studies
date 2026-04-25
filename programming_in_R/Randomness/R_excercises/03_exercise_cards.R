# You are playing an online card game that uses R to simulate shuffling of
# cards. The card game uses 52 cards, ranked 2 to 10 together with Jack (J),
# Queen (Q), King (K) and Ace (A); There are four suits named Clubs (C),
# Diamonds (D), Hearts (H), Spades (S). The cards are represented by strings
# naming the suit first, followed by the rank, e.g. "C4" (Four of Clubs) or "DA"
# (Ace of Diamonds). The unshuffled deck is thus:
cards.ordered <- paste0(
  rep(c("C", "D", "H", "S"), 13),
  rep(c(2:10, "J", "Q", "K", "A"), each = 4)
)
# To get a shuffled deck, the online game uses the following function. The seed
# is based on the time elapsed since 1970-01-01, 00:00:00 in milliseconds;
# however, because the seed is limited to numbers below about 2e9, only the last
# 9 digits of that time are used (i.e. the rest from a division by 1e9 is used).
# The time elapsed since 1970-01-01, 00:00:00 is a very typical unit for time
# measurement in computer systems: `as.numeric(Sys.time())`, for example,
# returns the number of *seconds*. Our demo code here therefore multiplies this
# with 1000.
shuffleCards <- function() {
  milliseconds <- as.numeric(Sys.time()) * 1000
  set.seed(milliseconds %% 1e9)
  cards.ordered[sample.int(52)]
}
# At the beginning of each game, the first five cards of the shuffled deck are
# revealed, e.g.
# > cards.shuffled <- shuffleCards()
# > cards.shuffled[1:5]
#
# Write a function that infers the entire shuffled deck (`cards.shuffled` in the
# example) from the revealed cards and an estimate of when the `shuffleCards()`
# call was executed on the server. You do not know the exact time of the server
# on which the cards are shuffled, and there seems to be some variation caused
# by network delays and caching. However, you are reasonably sure that you can
# estimate the server's clock by plus/minus 2 seconds. You can therefore try out
# different candidate seeds that the server could have used, set it, and do the
# same operation as `shuffleCards()`, to see if you get the same first 5 cards.
#
# Unlike in some other exercises, you explicitly need to call `set.seed()` in
# this exercise.
#
# Inputs:
#  - `cards.shuffled.head` (`character(5)`): A vector of length 5 containing a
#   subset of `cards.ordered`, indicating the first five cards revealed in the
#   game.
#  - `time.estimate` (`POSIXct`): An estimate of the time at which the
#   `shuffleCards()` function was executed. (Do not call `Sys.time()` in your
#   solution, because there could be a delay between when the cards are shuffled
#   and when the first five cards are revealed). The time used is to create
#   `cards.shuffled.head` is off by at most plus or minus 2 seconds (2000
#   milliseconds). You can use `assertPOSIXct()` to check for validity of this
#   argument.
# Return value: A `character(52)` vector containing a guess at the complete
# deck. If no possible seed in the possible time window around `time.estimate`
# could have generated `cards.shuffled.head`, then an error should be thrown.
#
# Your function can make use of the `cards.ordered` variable defined above.
#
# If your function works, then you can try it out by running the following two
# lines in quick succession:
# cs <- shuffleCards()
# ex01InferCards(cs[1:5], Sys.time())
# cs
# which should return the same vector as `cs`.
ex01InferCards <- function(cards.shuffled.head, time.estimate) {
  # your code
  assertCharacter(cards.shuffled.head, len = 5, any.missing = FALSE)
  assertPOSIXct(time.estimate, any.missing = FALSE, len = 1)

  cards.ordered <- paste0(
    rep(c("C", "D", "H", "S"), 13),
    rep(c(2:10, "J", "Q", "K", "A"), each = 4)
  )

  lower <- round(as.numeric(time.estimate - 2) * 1000)
  upper <- round(as.numeric(time.estimate + 2) * 1000)
  miliseconds <- lower:upper
  for (i in miliseconds) {
    set.seed(i %% 1e9)
    guess <- cards.ordered[sample.int(52)]
    if (all(head(guess, 5) == cards.shuffled.head)) {
      return(guess)
    }
  }
  stop("no mathcing seed found in time window")
}
