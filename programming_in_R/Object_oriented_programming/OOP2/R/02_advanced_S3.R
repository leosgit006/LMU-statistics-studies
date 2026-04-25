
# The following is a constructor for an S3 representation of a "discrete set"
# (of real numbers).  It represents a set of numbers.
SetDiscrete <- function(content) {
  assertNumeric(content, any.missing = FALSE)
  structure(
    list(content = unique(content)),
    class = c("SetDiscrete", "Set")
  )
}

# For the "Set"-class, we define the following generics:

# Give a string-representation for a set.
# Input:
# - `obj`: the object to represent
# Returns: `character(1)` a string giving a human-readable representation of the
# object.
repr <- function(obj) {
  UseMethod("repr")
}

# For classes that do not have a repr-function, we return a boring string.
repr.default <- function(obj) {
  "Representation not implemented."
}

# Determine if `object` is a member of `set`.
# Input:
# - `set`: The set that is checked whether `object` is contained.
# - `object`: `numeric(1)`: The object to query.
# Returns: `logical(1)`: `TRUE` if `object` is in `set`, `FALSE` otherwise.
isElement <- function(set, object) {
  assertNumber(object)
  UseMethod("isElement")
}

# The implementation for the SetDiscrete are as follows:

# Note no assert is necessary for `obj`, since this method is only called when
# `obj` is a `SetDiscrete`.
repr.SetDiscrete <- function(obj) {
  paste0("{", paste(obj$content, collapse = ", "), "}")
}

# Note no assert is necessary for `set`, as above. Also no assert is necessary
# for `object`, since it is already asserted in the `isElement` generic
# function.
isElement.SetDiscrete <- function(set, object) {
  object %in% set$content
}

# Using the repr, we can also make a nice printer.
# This is implemented for *all* Set-objects.
print.Set <- function(x, ...) {
  cat(sprintf("A Set:\n%s\n", repr(x)))
  invisible(x)  # print.XXX must always return `invisible(x)`!
}


# If you source the file until here, you should be able to type in the
# following:
# > SetDiscrete(c(3, 1, 2, 3, 2, 1))
# And get the following output:
# #> A Set:
# #> {3, 1, 2}
# Also
# > isElement(SetDiscrete(c(1, 2)), 1)  # TRUE
# > isElement(SetDiscrete(c(1, 2)), 3)  # FALSE

# ------------------------------------------------------------------------------
# Write a constructor, and an implementation, of a `SetInterval` class.
# You will have to implement the functions `SetInterval()`,
# `repr.SetInterval()`, and `isElement.SetInterval()`.

# Exercise 01: SetInterval
#
# `SetInterval()` should work as follows:
# Inputs:
# - `lower`: `numeric(1)`, indicating the lower (inclusive) bound of the
#   interval.
# - `upper`: `numeric(1)`, indicating the upper (inclusive) bound of the
#   interval.
# Returns: An object of class "SetInterval", as well as "Set".
# An error should be given if `lower` is greater than `upper`.
SetInterval <- function(lower, upper) {
  assertNumber(lower)
  assertNumber(upper)
  if (lower > upper) stop("lower must not be greater than upper")
  structure(
    list(lower = lower, upper = upper),
    class = c("SetInterval", "Set")
  )
}

# Exercise 02: repr.SetInterval
#
# The representation of a `SetInterval` with bounds `lower` and `upper` should
# be of the form `"[lower, upper]".
#
# Examples:
# > repr(SetInterval(-1, 1))
# #> [1] "[-1, 1]"
# > repr(SetInterval(0, 0))
# #> [1] "[0, 0]"
repr.SetInterval <- function(obj) {
  paste0("[", obj$lower, ", ", obj$upper, "]")
}

# Exercise 03: isElement.SetInterval
#
# An object is an element of a SetInterval(lower, upper) if it falls between
# these bounds, inclusive.
# > isElement(SetInterval(-1, 1), 0)  # TRUE
# > isElement(SetInterval(-1, 1), -1)  # TRUE
# > isElement(SetInterval(-1, 1), 1)  # TRUE
# > isElement(SetInterval(-1, 1), 1.01)  # FALSE
# > isElement(SetInterval(-1, 1), -100)  # FALSE
isElement.SetInterval <- function(set, object) {
  object >= set$lower & object <= set$upper
}


# Write a constructor, and implementation, of a `SetUnion`, and a
# `SetIntersection` class.  These represent sets that are unions / intersections
# of other sets, which need to be contained in the `SetUnion` /
# `SetIntersection` objects.  The `repr()` and `isElement()` implementations for
# these classes need to call `repr()` and `isElement()` on the contained sets to
# produce their results.

# Exercise 04: SetUnion
#
# `SetUnion()` should work as follows:
# Inputs:
# - `set1`: an object of class `Set`. You can use `assertClass()` here.
# - `set2`: an object of class `Set`.
# Returns: An object of class "SetUnion", as well as "Set".
SetUnion <- function(set1, set2) {
  assertClass(set1, "Set")
  assertClass(set2, "Set")
  structure(
    list(set1 = set1, set2 = set2),
    class = c("SetUnion", "Set")
  )
}

# Exercise 05: repr.SetUnion
#
# The representation of a `SetUnion` with sets `set1` and `set2` should be of
# the form `"(set1 U set2)".
#
# You will have to call `repr()` on the objects in your set for this.
#
# Examples:
# > repr(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))))
# #> [1] "({1, 2} U {1, 3})"
# > repr(SetUnion(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 0)
#   ))
# #> [1] "(({1, 2} U {1, 3}) U [-1, 0])"
repr.SetUnion <- function(obj) {
  paste0("(", repr(obj$set1), " U ", repr(obj$set2), ")")
}

# Exercise 06: isElement.SetUnion
#
# An object is an element of a SetUnion(set1, set2) if it is an element of *at
# least one* of the sets contained in it.
#
# You will have to call `isElement` on the object in your set for this.
#
# > isElement(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 1)  # TRUE
# > isElement(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 3)  # TRUE
# > isElement(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 4)  # FALSE
# > isElement(SetUnion(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), -0.5)  # TRUE
# > isElement(SetUnion(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), -1.5)  # FALSE
isElement.SetUnion <- function(set, object) {
  isElement(set$set1, object) | isElement(set$set2, object)
}

# SetIntersection() is very similar to SetUnion, except for the fact that an
# an object is only element of an intersection if it contained in *both* sets.

# Exercise 07: SetIntersection
#
# `SetIntersection()` should work as follows:
# Inputs:
# - `set1`: an object of class `Set`. You can use `assertClass()` here.
# - `set2`: an object of class `Set`.
# Returns: An object of class "SetInterval", as well as "Set".
SetIntersection <- function(set1, set2) {
  assertClass(set1, "Set")
  assertClass(set2, "Set")
  structure(
    list(set1 = set1, set2 = set2),
    class = c("SetIntersection", "Set")
  )
}


# Exercise 08: repr.SetIntersection
#
# The representation of a `SetIntersection` with sets `set1` and `set2` should
# be of the form `"set1 /\\ set2".
#
# Examples:
# > repr(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))))
# #> [1] "{1, 2} /\\ {1, 3}"
# > repr(SetIntersection(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 0)
#   ))
# #> [1] "(({1, 2} U {1, 3}) /\\ [-1, 0])"
#
# Note that we are using two backslashes here, because the backslash is an
# escape character.
#
# Like this, we should get:
# > print(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))))
# #> {1, 2} /\ {1, 3}
repr.SetIntersection <- function(obj) {
  paste0("(", repr(obj$set1), " /\\ ", repr(obj$set2), ")")
}

# Exercise 09: isElement.SetIntersection
#
# An object is an element of a SetIntersection(set1, set2) if it is an element
# of *both* of the sets contained in it:
# > isElement(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 1)  # TRUE
# > isElement(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 3)  # FALSE
# > isElement(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 4)  # FALSE
# > isElement(SetIntersection(
#     SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), 1)  # TRUE
# > isElement(SetIntersection(
#     SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), 0)  # FALSE
isElement.SetIntersection <- function(set, object) {
  isElement(set$set1, object) & isElement(set$set2, object)
}
