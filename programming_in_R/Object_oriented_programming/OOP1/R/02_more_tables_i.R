# Write a function that checks whether one data.table is the row-reordered form of another.
# We call this "equivalent" here. All tables must contain an `id` column (absence of which
# is an error), the content of which should be ignored for the purpose of checking equivalence.
#
# Inputs:
# - a, b: `data.table` with atomic (non-list) columns
# Output: `logical(1)` (i.e. `TRUE` or `FALSE`)
#
# Two tables are equivalent whenever one could reorder one table's rows to get the other table,
# ignoreing the content of the `id` column.
#
# The following should be `TRUE`:
# ex01TablesEquivalent(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = c(2, 1, 2), b = c("b", "a", "b"), id = c(4, 5, 6))
# )
#
# The following should be `FALSE`:
# ex01TablesEquivalent(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = c(1, 1, 2), b = c("a", "a", "b"), id = c(4, 5, 6))
# )
#
# Tables having different column names, columns in different order or columns of the
# same name with different types are always not equivalent (even if this concerns the `id` column).
# Tables with different numbers of rows can also not be equivalent.
#
# You can use `fsetequal` or `all.equal` here.
ex01TablesEquivalent <- function(a, b) {
  # your code
  assertDataTable(a, any.missing = FALSE, types = "atomic")
  assertNames(names(a), must.include = "id")

  assertDataTable(b, any.missing = FALSE, types = "atomic")
  assertNames(names(b), must.include = "id")

  if (!identical(names(a), names(b))) {
    return(FALSE)
  }
  ## checks name and order of cols, nut not for type

  classes.a <- vapply(a, class, character(1))
  classes.b <- vapply(b, class, character(1))
  if (!identical(classes.a, classes.b)) {
    return(FALSE)
  }
## checks for identical classes of cols

  if (!identical(nrow(a), nrow(b))) {
    return(FALSE)
  }
  ## checks for same number of rows

  a.content <- a[, !"id"]
  b.content <- b[, !"id"]

  if (!fsetequal(a.content, b.content)) {
    return(FALSE)
  }
  ## checks for identical content
  return(TRUE)
}

# Write a function that returns all rows in `a` that also occur in `b`, ignoring the `id` column.
#
# Inputs:
# - a, b: `data.table` with atomic (non-list) columns
# Output: `data.table`
#
# Similarly to ex01TablesEquivalent, `a` and `b` contain an `id` column that should be ignored for
# the purpose of checking whether rows are the same (but the `id` of the table `a` should be included
# in the output!).
# Also, just like in ex01TablesEquivalent, different column names, order, or types (even regarding
# the `id` col) mean no rows are equal (and an empty version of `a`, e.g. `a[FALSE, ]` should be returned).
#
# Example:
# ex02TablesIntersect(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = 2, b = "b", id = 10)
# )
# --> data.table(a = c(2, 2), b = c("b", "b"), id = c(2, 3))  # returning rows 2 and 3 of `a`, since they
#                                                             # are also found in `b`
#
# This can be solved using a "join", using `merge()` or `[... on = ...]`. Be careful that, when using
# the letters `a` and `b` within `[ ... ]`, you know whether they reference the function arguments `a`/`b`,
# or the columns within the table that could also be `a` or `b`.
ex02TablesIntersect <- function(a, b) {
  # your code
  assertDataTable(a, any.missing = FALSE, types = "atomic")
  assertNames(names(a), must.include = "id")
  ## checking a

  assertDataTable(b, any.missing = FALSE, type = "atomic")
  assertNames(names(b), must.include = "id")
  ## checking b

  if (!identical(names(a), names(b))) {
    return(a[FALSE, ])
  }
  ## checks name and order of cols, but not for typeof

  classes.a <- vapply(a, class, character(1))
  classes.b <- vapply(b, class, character(1))
  if (!identical(classes.a, classes.b)) {
    return(a[FALSE, ])
  }
  ## checks for identical classes of cols

  a.content <- a[, !"id"]
  b.content <- b[, !"id"]
  ## for clean content

  cols <- setdiff(names(a), "id")
  b.subset <- unique(b[, cols, with = FALSE])
  res <- a[b.subset, on = cols, nomatch = NULL]
  return(res)

}


# Write a function that reads in a `data.table` from a file.
#
# Inputs:
# - `fname`: file name (relative to the repository base directory)
# Output: `data.table`
#
# The file format is: The first three rows of the file contain
# comments and should be skipped. They are followed by a row of
# column names, and then data. Columns are separated by semicolon (`;`) and
# should be read either as `numeric` or `character` columns, as appropriate.
# A string `MISSING` indicates the value should be NA.
# If a column data contains at least one value with at least one comma (`,`), this
# column should be a list column with character vectors of possibly variable
# length as values.
#
# Consider the `example.csv` file, which should give rise to the following
# behaviour:
# ex03ReadTable(file.path("R", "example.csv"))
# -->
# data.table(
#   species = c("cat", "dog", "eldritch"),
#   names = list(c("Felix", "Sir Cattington", "Larry"), c("Maggie", "Snuffles", "Klaus"),
#     c("Yaldabaoth", "Behemoth", "The Other")),
#   ages = list(c("3", "7", "4"), c("1", "4", "3"), c("10234", "5231", "13792168024")),
#   "food, class, if known" = c("Cat Food", "Dog Food", NA),
#   "cost, approximate, if known" = c(100, 250, NA)
# )
#
# Note the `names` and `ages` columns contain lists of characters only, while the `cost` column is numeric.
#
# Here you should use `fread` but you also need to do some post-processing.
ex03ReadTable <- function(fname) {
  assertString(fname)

  # Erste 3 Zeilen überspringen, Semikolon-getrennt, "MISSING" -> NA
  dt <- fread(fname, skip = 3, sep = ";", na.strings = "MISSING", colClasses = "character")

  # Jede Spalte nachverarbeiten
  for (col in names(dt)) {
    vals <- dt[[col]]

    if (any(!is.na(vals) & grepl(",", vals))) {
      # List-Spalte: Werte an Komma splitten
      set(dt, j = col, value = lapply(vals, function(x) {
        if (is.na(x)) NA_character_
        else strsplit(x, ",")[[1]]
      }))
    } else {
      # Numeric wenn möglich, sonst character
      nums <- suppressWarnings(as.numeric(vals))
      if (all(is.na(nums) == is.na(vals))) {
        set(dt, j = col, value = nums)
      }
      # sonst bleibt character
    }
  }

  dt
}
