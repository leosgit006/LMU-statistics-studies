
# Write a function that normalizes date formats.
#
# Your function gets a vector of strings that my contain dates in various
# formats:
# - `YYYY-MM-DD`, e.g. "2023-12-01"
# - `YY-MM-DD`: "23-12-01"
# - `DD.MM.YYYY`: "01.12.2023"
# - `DD.MM.YY`: "01.12.23"
# - `MM/DD/YYYY`: "12/01/2023"
# - `MM/DD/YY`: "12/01/23"
#
# Your function should return the vector, where all occurrences of dates in
# these formats were converted to the `YYYY-MM-DD` format (a.k.a. the "correct
# format").
#
# It is not necessary to check for correctness of dates (i.e. silently accept
# "2023-13-32").
#
# When converting years from `YY` to `YYYY`, you should make use of the argument
# `century.cutoff` -- years years that are below or equal to the two-digit
# `century.cutoff` year should be converted to `20YY`, years above
# `century.cutoff` should be converted to `19YY`.
#
# Inputs:
# - `strings`: a `character` vector with no missing values, potentially
#   containing dates in different formats. Each entry of this vector may contain
#   multiple dates, and their format may differ.
# - `century.cutoff`: An integer `numeric(1)` between 0 and 99 inclusive,
#   indicating for which year to count a two-digit year as being in the 21st
#   century.
#
# Return: A `character` vector, with the same length as `strings`, with
# converted dates.
#
# The following input:
example.datestrings <- c("01/23/45",
  "He was born on 22.11.31 and died on 01.06.30 at the age of 98.",
  "This happened on 12/11/19, not on 12/11/1919.",
  "'You went on a date?' -- 'Yes, I went on 12/12/22, which is a date'",
  "",
  "The YYYY-MM-DD format can be naturally sorted, making it superior to DD.MM.YYYY.",
  "She was born on '01.02.03', but some people think she was born on '01/02/03'.")
# > ex01DateConvert(example.datestrings, century.cutoff = 30)
# should give:
# c("1945-01-23",
#   "He was born on 1931-11-22 and died on 2030-06-01 at the age of 98.",
#   "This happened on 2019-12-11, not on 1919-12-11.",
#   "'You went on a date?' -- 'Yes, I went on 2022-12-12, which is a date'",
#   "",
#   "The YYYY-MM-DD format can be naturally sorted, making it superior to DD.MM.YYYY.",
#   "She was born on '2003-02-01', but some people think she was born on '2003-01-02'.")
ex01DateConvert <- function(strings, century.cutoff) {
  # your code
  assertCharacter(strings, any.missing = FALSE)
  assertInt(century.cutoff, lower = 0, upper = 99, tol = 0)

  # Do y-m-d first, otherwise we handle these twice
  # (after having converted the others).
  # Also we need \\d{4}, otherwise YYYY-MM-DD is seen as
  # YY-MM-DD ignoring the first two YY
  # (but there are other ways for this)
  patterns.dateformats <- c(
    "(?<year>\\d{4}|\\d{2})-(?<month>\\d{2})-(?<day>\\d{2})",
    "(?<day>\\d{2})\\.(?<month>\\d{2})\\.(?<year>\\d{4}|\\d{2})",
    "(?<month>\\d{2})/(?<day>\\d{2})/(?<year>\\d{4}|\\d{2})"
  )

  for (pattern in patterns.dateformats) {

    matches <- regmatches(strings, gregexec(pattern, strings, perl = TRUE))

    matches.normalized <- lapply(matches, function(m) {
      if (!length(m)) return(character(0))
      days <- m["day", ]
      months <- m["month", ]
      years <- m["year", ]
      years <- ifelse(nchar(years) == 2,
                      ifelse(as.numeric(years) > century.cutoff,
                             paste0("19", years),
                             paste0("20", years)
                      ),
                      years
      )
      sprintf("%s-%s-%s", years, months, days)
    })

    regmatches(strings, gregexpr(pattern, strings, perl = TRUE)) <- matches.normalized
  }

  strings
}

# Write a function that checks whether given equations are correct or not.  An
# equation is given as a character string and will only consist of the
# operations '+', '-', and '='. You can rely on the fact that there will only be
# a '=' present, but both sides may contain multiple operations.  There will
# also not be any 'x + (-3)' or similar, that will always just be 'x - 3'.
#
# The input 'equations' shall be a character vector of separate equations.  Your
# function should return a logical vector indicating if the equations were
# correct or not.  As an example, the return for an input of:
example.equations <- c("3 + 4 - 10 = -3", "3+5 -2 + 2 =12", "9 - 2 + 4 = 0",
                       "2 =5", "3-4 = 5 - 6", "-30 + 40 = 10")
# should be 'c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)'
#
# NOTE: You should not use functions like `parse()`, `str2lang()` or
# `str2expression()` here. The use of `eval(parse())` is generally bad practice
# that you shouldn't learn, and would also miss the purpose of this exercise. It
# is not a solution accepted by the tests.  Functions like `?do.call()` or
  # `?Reduce()` may be helpful here.
#
# NOTE 2: R is not able to represent decimal numbers exactly, which can lead to
# small deviations due to imprecision:
# > -.1 == 1 - 1.1
# #> [1] FALSE
# You should therefore accept both sides of the equation as equal when they
# differ by less than about 10^-8.
ex02EquationCheck <- function(equations) {
  # your code
  assertCharacter(equations, any.missing = FALSE)

  equations <- gsub(" ", "", equations)

  evalEqnPart <- function(eqn.part) {
    pattern <- "[+-]?[^+-]+"
    numbers <- regmatches(eqn.part, gregexpr(pattern, eqn.part))[[1]]
    sum(as.numeric(numbers))
  }
  vapply(strsplit(equations, "="), function(eqn.parts) {
    lhs <- evalEqnPart(eqn.parts[[1]])
    rhs <- evalEqnPart(eqn.parts[[2]])
  abs(lhs - rhs) < 1e-8
  }, logical(1))
}
