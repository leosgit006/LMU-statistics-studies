
# Write a function that behaves like a simple version of `lapply()`: It applies
# a given `FUN` to a list or vector `X`. However, if `FUN` throws an error,
# instead of aborting, the function should impute a value `impute`.
#
# This function gets three arguments:
# - `X`: Any kind of object that `lapply()` can iterate over. It is not
#   necessary to use a `checkmate` assert on this argument.
# - `FUN`: A function taking one argument.
# - `impute`: The value to use in cases where `FUN` throws an error.  This can
#   be any type, it is not necessary to use a `checkmate` assert here.
#
# > ex01SafeLapply(c(2, 16, 64), log2, impute = -1)  # list(1, 4, 6)
# > ex01SafeLapply(list(2, 16, "x"), log2, impute = -1)  # list(1, 4, -1)
# > ex01SafeLapply(list(2, 16, "x"), log2, impute = NULL)  # list(1, 4, NULL)
#
# Be aware that the return value of `FUN` could be the result of a `try()` call,
# so you should likely use `tryCatch()` instead of `try()` here:
# > ex01SafeLapply(list(1, NULL, try(stop("."), silent = TRUE)), identity, impute = -1)
# ## returns `list(1, NULL, try(stop("."), silent = TRUE))` -- -1 is not imputed
# ## here.
ex01SafeLapply <- function(X, FUN, impute) {
  # your code
  assertFunction(FUN)

  lapply(X, function(x.i) {
    tryCatch({
      FUN(x.i)
    }, error = function(e) {
      impute
    })
  })
}
