
# In this exercise, we are going to build our own simplified implementation of `lm()`.
# In our version we are only going to do univariate regression, intercept optional.
# However, we will try to implement the most important methods that R offers for linear models.
#
# What can `?lm()` do? Consider the `cars` dataset: It records the "speed of cars and the distance
# taken to stop", according to `?cars`.
#
# > print(head(cars))
#
# We can fit a linear model, print basic information, coefficients and make predictions in the following way:
#
# > model <- lm(dist ~ speed, data = cars)
# > print(model)
# > coef(model)
# > speed <- seq(0, 25, by = 0.5)
# > dist.predicted <- predict(model, newdata = data.frame(speed = speed))
# > plot(cars)
# > points(speed, dist.predicted, col = "red")
#
# Note that `model` has the `"lm"` class.
# The following lists all S3-methods implemented for that class.
#
# > class(model)
# > methods(class = "lm")
#


# Exercise 1: `ulm()`: a Univariate Linear Model
#
# Write a function `ulm()` that performs univariate linear regression.
# Do not use the `lm()`-function, write your own regression method!
#
# Your function should have the following arguments:
#    `x` - `numeric` vector: the covariate / independent variable, with at least two elements.
#    `y` - `numeric` vector: the outcome / dependent variable, same length as `x`.
#    `intercept` - `logical(1)`: whether to add an intercept, defaulting to `TRUE`.
#
# As a reminder: Univariate linear regression with nonzero intercept estimates the slope as
# `cov(x, y) / var(x)` and the intercept as `mean(y) - slope * mean(x)`.
# Univariate linear regression with zero intercept estimates the slope as `sum(x * y) / sum(x^2)`.
#
# The return value of the `ulm()`-function should be an S3-object of class `"ulm"`.
# Think about what information to include in that returned object.
# A good approach is to return a very basic object at first.
# Then, while you are writing the other necessary functions (`print()`, `coef()`, ...),
# come back here and edit the `ulm()` function to return more things.

ulm <- function(x, y, intercept = TRUE) {
  assertNumeric(x, any.missing = FALSE, min.len = 2)
  assertNumeric(y, len = length(x), any.missing = FALSE)
  assertFlag(intercept)
  # your code
  if (intercept) {
    s <- cov(x, y) / var(x)
    i <- mean(y) - s * mean(x)
    co <- c("intercept" = i, "slope" = s)
  } else {
    s <- sum(x * y) / sum(x^2)
    co <- c("intercept" = 0, "slope" = s)
  }
  res <- list(coefficients = co, has.intercept = intercept)
  class(res) <- "ulm"
  res
}


# Exercise 2: `print()`
#
# Write a function that implements the `print()`-method for your `"ulm"`-class and prints
# the result of an `ulm()` call.
#
# The output for `ulm(cars$speed, cars$dist)` should be:
#
###
# Univariate Linear Model
#
# intercept      slope
# -17.579095   3.932409
###
#
# The output for `ulm(cars$speed, cars$dist, intercept = FALSE)` should be:
#
###
# Univariate Linear Model without Intercept
#
# slope
# 2.909132
###

print.ulm <- function(x, ...) {
  # your code

  if (x$has.intercept) {
    cat("Univariate Linear Model\n\n")
    print(x$coefficients)
  } else {
    cat("Univariate Linear Model without Intercept\n\n")
    print(x$coefficients["slope"])
  }
}


# Try it out:
#
# > ulm(cars$speed, cars$dist)
#
# Expected output:
###
# Univariate Linear Model
#
# intercept      slope
# -17.579095   3.932409
###
#
# > ulm(cars$speed, cars$dist, intercept = FALSE)
#
# Expected output:
###
# Univariate Linear Model without Intercept
#
# slope
# 2.909132
###


# Exercise 3: `coef()`
#
# Implement the `coef()` method for your `"ulm"`-class.
# It should return a named vector with two elements, named `"intercept"` and `"slope"`.
# The `"intercept"` value should be 0 for models fitted with `intercept = FALSE`.
#
# You may either write a function here, or you can try to solve this problem without writing a function.
# All we require is that `coef(ulm(cars$speed, cars$dist))` equals
# `c(intercept = -17.5790948905109339, slope = 3.9324087591240868)`.



# Try it out:
#
# > coef(ulm(cars$speed, cars$dist, intercept = FALSE))
#
# Expected output:
###
# intercept    slope
# 0.000000  2.909132
###


# Exercise 4: `predict()`
#
# Implement the `predict()`-method for your `"ulm"`-class.
# Your function should take the model itself, as well as `newdata`: a `data.frame`.
# The first column of `newdata` should be used to make predictions, the other columns should be ignored.
#
# The return value should be `numeric` vector, named by `rownames(newdata)`, containing the predictions.

predict.ulm <- function(object, newdata, ...) {
  # your code
  x <- coef(object)
  used.data <- newdata[[1]]
  res <- x[["intercept"]] + x[["slope"]] * used.data
  names(res) <- rownames(newdata)
  res
}


# Try it out:
#
# > model.ul <- ulm(cars$speed, cars$dist)
# > predict(model.ul, newdata = head(cars))
#
# Expected output:
###
# 1         2         3         4         5         6
# -1.849460 -1.849460  9.947766  9.947766 13.880175 17.812584
###
