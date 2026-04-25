# This exercise concerns itself with the *same* flight data as
# `02_exercise_flights_i.R`. Read the first paragraph in that R-file for more
# information about the format. Otherwise, these exercises are independent from
# the exercises in `02_exercise_flights_i.R` and can be solved on their own.


# Write a function that counts the number of flights on each route that departed
# each month until a flight on that route was delayed more than 60 minutes.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `flights.to.delay`, `origin`, `dest`.
# Similarly to `ex02MonthlyDelays`, this function should aggregate across flight
# routes and months in a given given year.
# Rows should be chronologically ordered.
# For each route and month, the number
# of flights that departed from the start of that month until one flight's
# arrival was delayed more than 60 minutes should be given. The delayed flight
# should not be counted. So if 10 flights departed for a route in a given month
# and the 5th flight was delayed > 60 minutes, the return value would be 4. If
# no flight was delayed more than 60 minutes, the return value should be  the
# total number of flights on that route and month, i.e. 10 in this example. If
# no flight was on the given route during the month, the value is 0. Note that,
# as in `ex02MonthlyDelays`, the year could be different from the years in the
# `flights` dataset, which should result in a table full of `0`s.
#
# A possible return value of this function when called with `flights.data` and
# 2014 is saved in `"ex03FlightsToDelayResult.csv"`:
flights.finished <- fread("tables/ex03FlightsToDelayResult.csv")
ex01FlightsToDelay <- function(flights, year.arg) {
  assertDataTable(flights)
  assertIntegerish(year.arg)

  all.airports <- flights[, unique(c(origin, dest))]

  all.flights <- CJ(month = 1:12,
                    origin = all.airports,
                    dest = all.airports)[origin != dest]

  res <- flights[year == year.arg][order(month, day, hour),
                                   .(flights.to.delay = as.integer(min(c(which(arr_delay > 60), .N + 1),
                                                                       na.rm = TRUE) - 1)),
                                   by = .(month, origin, dest)][
                                     all.flights, on = .(month, origin, dest)]

  res[is.na(flights.to.delay), flights.to.delay := 0]

  res[, .(month, flights.to.delay, origin, dest)]
}

# Write a function that, for each month in a given year, and for each airline,
# calculates (1) the airline's mean flight delay, and (2) the flight delay of
# that airline's competition, i.e. every other airline's mean flight delay.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# - `year`: the year for which to aggregate. (Remember that `flights` could
#   contain data from multiple years.)
# Output:
# `data.table` with columns `month`, `carrier`, `mean.delay`,
# `mean.delay.competition`.
# The table should contain a row for each month and each each carrier, even
# if the given carrier (or its competition) do not have any flights on that
# month (in which case the respective `mean.delay` / `mean.delay.competition`
# should be 0).
# Rows should be chronologically ordered.
# A possible return value of this function when called with `flights.data`
# and 2014 is saved in `ex04CarrierDelayResult.csv`:
flights.carrierdelay <- fread("tables/ex04CarrierDelayResult.csv")
# As in other tasks, the `CJ()` function can be handy here to create a
# table of for all carriers within each month.
ex02CarrierDelay <- function(flights, year) {
  # your code
  assertDataTable(flights)
  assertIntegerish(year, len = 1)

  year.arg <- year
  ## so year doesn`t get mixed up

  all.airlines <- unique(flights$carrier)
  cross.join <- CJ(month = 1:12, carrier = all.airlines)
## create variables
  all.airlines
  right.flights <- flights[year %in% year.arg]

  res <- right.flights[,
                      .(mean.delay = mean(arr_delay), mean.delay.competition = mean(arr_delay)),
                      by = c("month", "carrier")][
                        cross.join, on = c("carrier", "month")][,
                                                  .(mean.delay, mean.delay.competition = right.flights[
                                                    month == .BY$month & carrier != .BY$carrier, mean(arr_delay)]),
                                                  by = c("month", "carrier")]
  res[is.na(mean.delay), mean.delay := 0]
  res[is.na(mean.delay.competition), mean.delay.competition := 0]
  setcolorder(res, c("month", "carrier", "mean.delay", "mean.delay.competition"))
  return(res[])
}
