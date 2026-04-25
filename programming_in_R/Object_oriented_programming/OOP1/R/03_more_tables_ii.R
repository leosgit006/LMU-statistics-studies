
# Write a function that `cbind`s two data.tables, avoiding duplicate column
# names in a certain way.
# Input:
# - table.a: `data.table`
# - table.b: `data.table`
# Output: Return a `data.table` that contains all the columns of `table.a`,
# followed by the columns of `table.b`, in order. However, should a column
# name occur in `table.b` that is already in `table.a`, then the column
# should be suffixed by `_copy`. If that *also* leads to a name collision,
# it should be suffixed instead by `_copy.1` (or `_copy.2` etc.)
# You can rely on `table.a` and `table.b` for themselves having unique column
# names, but some of their column names may *already* end on `_copy` or
# `_copy.#`.
#
# If `table.a`. and `table.b` have a different number of rows, an error
# should be thrown.
# Example input:
table.a.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4)
table.b.example <- data.table(z = 100, x = 200, y = 300, y_copy.1 = 400, y_copy.2 = 500)
# This should give the output
table.ab.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4,
  z = 100, x_copy = 200, y_copy.3 = 300, y_copy.1_copy = 400, y_copy.2 = 500)
# (note that `y` gets renamed to `y_copy.3` because `_copy`, `_copy.1` and
# `_copy.2` are already taken; `y_copy.1` of `table.b` turns into
# `y_copy.1_copy`, because `y_copy.1` is already present in `table.a`.)
ex01CbindNameClash <- function(table.a, table.b) {
  assertDataTable(table.a)
  assertDataTable(table.b)

  if (nrow(table.a) != nrow(table.b)) {
    stop("Row count mismatch")
  }

  table.b.copy <- copy(table.b)
  current.names.a <- names(table.a)
  original.names.b <- names(table.b.copy)
  new.names.b <- character(length(original.names.b))

  for (i in seq_along(original.names.b)) {
    clean.name <- original.names.b[i]
    candidate <- clean.name
    counter <- 0

    while (candidate %in% current.names.a) {
      if (counter == 0) {
        candidate <- paste0(clean.name, "_copy")
      } else {
        candidate <- sprintf("%s_copy.%d", clean.name, counter)
      }
      counter <- counter + 1
    }

    new.names.b[i] <- candidate
    current.names.a <- c(current.names.a, candidate)
  }

  setnames(table.b.copy, new.names.b)
  return(cbind(table.a, table.b.copy))
}