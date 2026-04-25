# This exercise, and the following exercises, concern themselves with data in a certain format.
#
# Setting: You are a data science consultant, hired to help the WidgetCorp (R) corporation
# analyse its production facilities for Widgets (TM). WidgetCorp (R) has a number of
# Widget (TM) production machines that are lovingly called Machine01, Machine02, Machine03, etc.
# Each of these machines has the same set of Sensors, measuring temperature, humidity,
# electricity consumption, water inflow rate, water purity, and various other metrics.
# Because the particulars of the quantities being measured are not relevant, these sensors
# are just enumerated as sensor01, sensor02, sensor03, etc.
# You are given a dataset that records, for each produced Widget (TM), (1) the Machine that
# produced it, (2) the quality of the Widget (TM), on a scale from 0 to 100, as assessed by
# certified Widget (TM) Quality Assessment Professionals, and (3) the values of the various
# sensor readings when the Widget (TM) was produced.
# However, the sensors have a limited measurement range that sometimes gets undercut. E.g.
# if the electricity consumption is below a certain threshold, the sensor is not able to
# record that value and just records NA.
# Example datasets could look like the following.
# (Note the `rbindlist`-layout is only for readability here. You can run the following code and
# will get a proper `data.table` from it.)
widget.corp.data <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine01",    78,             23,              28.6,            -23,             NA),
  list("Machine02",    28,             41,              77.8,            NA,              27),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine03",    80,             NA,              32.3,            NA,              NA),
  list("Machine03",    58,             10,              77.8,            3,               NA),
  list("Machine02",    74,             NA,              24.5,            -18,             3),
  list("Machine01",    46,             81,              NA,              NA,              NA),
  list("Machine01",    24,             43,              13.3,            -22,             NA),
  list("Machine02",    7,              96,              96.0,            0,               NA),
  list("Machine01",    22,             107,             23.5,            7,               8),
  list("Machine03",    98,             NA,              NA,              11,              NA)
))
# In the following tasks you should write functions that handle data just like this. However, you may get
# a dataset with more or fewer machines, and with more or fewer sensors.

# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.

# Hint:
# You will probably write some functions that modify the input data in-place. This means that calling
# one of your functions may actually cause the input value to be different after the call. Be aware of this
# if you try out your functions on the `widget.corp.data` example data. E.g. if you do
# > result <- ex04CleanTable(widget.corp.data)
# then `widget.corp.data` itself may have changed and could give different results in future experiments.
# You should therefore always run your functions on a `copy` of the input data, like so:
# > result <- ex04CleanTable(copy(widget.corp.data))
# Alternatively, you can just execute the above code-snippet (or source this .R-file) again after
# each experiment.

# Write a function that accepts one `data.table` argument `data` and sorts the given data according to
# the number of missing values in the `sensorXX` columns, in descending order. Ties should be broken
# by the `quality`, descending. Your function should return the `data.table` with rows sorted.
# The output for the example data would therefore be:
widget.corp.data.sorted <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine03",    98,             NA,              NA,              11,              NA),
  list("Machine03",    80,             NA,              32.3,            NA,              NA),
  list("Machine01",    46,             81,              NA,              NA,              NA),
  list("Machine01",    78,             23,              28.6,            -23,             NA),
  list("Machine02",    74,             NA,              24.5,            -18,             3),
  list("Machine03",    58,             10,              77.8,            3,               NA),
  list("Machine02",    28,             41,              77.8,            NA,              27),
  list("Machine01",    24,             43,              13.3,            -22,             NA),
  list("Machine02",    7,              96,              96.0,            0,               NA),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine01",    22,             107,             23.5,            7,               8)
))
ex01SortTable <- function(data) {
  # your code
  assertDataTable(data)

  dt <- copy(data)
  sensor.cols <- grep("^sensor", names(dt), value = TRUE)
  ## only sensors columns are selcted

  dt[, na.cols := rowSums(is.na(.SD)), .SDcols = sensor.cols]
## create na.cols column

  ordered.data <- setorder(dt, -na.cols, -quality)
## order the table by number of NAs and. quality

  ordered.data[, na.cols := NULL]
  return(ordered.data)
}
