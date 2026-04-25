# This exercise tests your knowledge about *basic features of data.table*
# Basically all of these functions should contain a few checkmate assertions,
# followed by very few lines of code.
#
# Use checkmate `assertDataTable` or `assertDataFrame` to make assertions on
# data.table/data.frame arguments (depending on which one is named in the task
# description). Use `assertNames()` to check column names (as in the example
# to follow).
#
# Example: Write a function that removes rows from a data.table. Your function
# has three inputs:
# - `minuend`: a `data.table` with multiple columns, at least one of which is
#   named "index".
# - `subtrahend`: a `data.frame` or a `data.table` with exactly one column
#   named "index".
# - `do.nothing`: a scalar `logical`.
# If `do.nothing` is `TRUE`, then the `minuend` argument should be returned
# as-is. If `do.nothing` is `FALSE`, then a `data.table` should be returned
# with all rows removed that have a value of `"index"` that occurs in the
# `subtrahend` "index" column. Note that row order and duplicate rows of `minuend`
# (with index that does not occur in `subtrahend`) should be preserved.
#
# This could, for example, be solved with the following code:
demoSubtractRows <- function(minuend, subtrahend, do.nothing) {
  assertFlag(do.nothing)
  assertDataTable(minuend)
  assertNames(colnames(minuend), must.include = "index")
  assertDataFrame(subtrahend, ncol = 1)  # assertDataFrame accepts *both* data.table and data.frame, so it is ideal here
  assertNames(colnames(subtrahend), identical.to = "index")
  if (do.nothing) {
    minuend
  } else {
    minuend[!subtrahend, on = "index"]
  }
}

# If you have looked at some of the data.table material then this function should
# be a breeze for you.


# Write a function that takes a list of named lists as input and creates a `data.table` as
# output, containing the input lists as rows.
# Inputs:
# - lst: `list` of named `lists` containing numeric scalars.
# Output: `data.table`
# The resulting `data.table` should contain the input rows in order and have columns according
# to the names of the input list (ordering of columns does not matter).
#
# Example:
# ex01List2DT(list(list(a = 1, b = 2), list(b = 3, a = 4)))
# --> data.table(a = c(1, 4), b = c(2, 3))
# Some lists do not contain elements for all columns; in that case, a 0 should be used:
# ex01List2DT(list(list(a = 1, b = 2), list(a = 4, c = 5)))
# --> data.table(a = c(1, 4), b = c(2, 0), c = c(0, 5))
# if there are elements in the lists that are not non-NA scalar numerics, an error should be thrown.
#
# You should probably use `?rbindlist` and possibly `?nafill` / `setnafill` here.
ex01List2DT <- function(lst) {
  assertList(lst, types = "list")

  # Check that all inner elements are non-NA scalar numerics
  for (inner in lst) {
    assertList(inner, names = "named")
    for (val in inner) {
      assertNumber(val, na.ok = FALSE)
    }
  }

  # rbindlist with fill=TRUE puts NA for missing columns -> replace with 0
  dt <- rbindlist(lst, fill = TRUE)
  setnafill(dt, fill = 0)
  dt
}
