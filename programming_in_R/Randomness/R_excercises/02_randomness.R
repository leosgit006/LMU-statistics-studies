
# You are studying the effect that a specific food supplement has on the growth
# of mice. You do this by feeding the supplement to a small number of mice and
# measuring their weight in grams. You compare this to the weight that mice of
# the same species have after a control diet. You reject the null hypothesis of
# no effect if the p-value of the difference of average weights between the
# groups is below a threshold.
# There are not many mice in the treatment group, and there is another
# limitation: The weight of the treatment-group is measured in integer grams,
# Therefore, if a mouse actually weighs 10.33244524 grams, its reported value is
# rounded to 10.
#
# Write a function that simulates this experiment under the null-hypothesis,
# that the weight of the treatment group mice equals the weight of the control
# group mice on average, but is normally distributed around that average.
#
# Your lab has a large number of control mice, and you therefore know the
# average control group weight quite precisely; this is also the average
# treatment group weight under the null hypothesis. You can therefore simulate
# the experiment by drawing random mouse weights with a given mean and standard
# deviation. You need to incorporate the rounding in your simulations!
#
# Input values:
# - `weight.avg` : `numeric(1)` average weight of the control mice.
# - `weight.stdev`: standard deviation of mouse weights to use.
# - `treatment.size`: number of mice in the treatment group.
# Return value: Vector of length `treatment.size` of (positive, integer valued)
# simulated mouse weights.
ex01MouseWeightSim <- function(weight.avg, weight.stdev, treatment.size) {
  # your code
  assertNumeric(weight.avg, len = 1, any.missing = FALSE)
  assertNumeric(weight.stdev, len = 1, any.missing = FALSE)
  assertIntegerish(treatment.size, len = 1, any.missing = FALSE)

  c.n <- rnorm(treatment.size, mean = weight.avg, sd = weight.stdev)
  round.cn <- round(c.n)
  as.integer(round.cn)
}

# Continuation of ex01:
#
# Write a function that, given experimental data, calculates an approximate
# p-value.
#
# Wikipedia:
# > The p-value is the probability of obtaining test results at least as extreme
# > as the results actually observed, under the assumption that the null
# > hypothesis is correct.
#
# Do this by simulating experiments: Simulate experiments under the
# null-hypothesis (i.e. by calling your solution for ex01). Do this
# `simulation.rounds` times. See how often the difference between
# `control.weight.avg` and the average of mice in a simulated experiment is
# greater or equal to the actually observed difference between
# `control.weight.avg` and the average of `treatment.weights`.
#
# This is an estimate of the `p`-value, since it counts the fraction of cases
# under the null-hypothesis, in which the difference in averages between control
# and treatment group is at least as much as the observed difference.
# Input values:
# - `control.weight.avg` : `numeric(1)` average weight of the control mice. As
#   mentioned before, this value is known precisely.
# - `weight.stdev`: standard deviation of mouse weights to use.
# - `treatment.weights`: integer `numeric`: vector of weights of treatment mice.
# - `simulation.rounds`: number of experiments to simulate.
# Return return the estimated p-value as a `numeric(1)`.
ex02MouseWeightPVal <- function(control.weight.avg, weight.stdev, treatment.weights, simulation.rounds) {
  # your code
  assertNumber(control.weight.avg)
  assertNumber(weight.stdev)
  assertInt(simulation.rounds, lower = 1)
  assertIntegerish(treatment.weights, any.missing = FALSE)

  simulations <- replicate(simulation.rounds, {
                           one.sim <- ex01MouseWeightSim(control.weight.avg,
                                    weight.stdev,
                                    length(treatment.weights))
                           mean(one.sim)
    })
  abs.real <- abs(control.weight.avg - mean(treatment.weights))
  abs.sim <- abs(control.weight.avg - simulations)
  mean(abs.sim >= abs.real)
}
