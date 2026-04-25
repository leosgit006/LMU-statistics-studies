# This exercise concerns itself with data in the same format as 02_exercise_sort.R
# Remember, the example datasets could look like the following:
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

# Write a function that accepts one `data.table` argument `data` and collects all the sensor data into
# a single list column. This means, the resulting `data.table` should have columns `machine`, `quality`,
# `sensor`, and the `sensor` column should be a `list` column containing a named `numeric` vector
# containing all the non-`NA` sensor values with appropriate names. The order of the table should not be changed.
# The output for the example data would therefore be:
widget.corp.data.list <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor = NULL),
  list("Machine01",    78,             list(c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23))),
  list("Machine02",    28,             list(c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27))),
  list("Machine03",    32,             list(c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10))),
  list("Machine03",    80,             list(c(sensor02 = 32.3))),
  list("Machine03",    58,             list(c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3))),
  list("Machine02",    74,             list(c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3))),
  list("Machine01",    46,             list(c(sensor01 = 81))),
  list("Machine01",    24,             list(c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22))),
  list("Machine02",    7,              list(c(sensor01 = 96, sensor02 = 96.0, sensor03 = 0))),
  list("Machine01",    22,             list(c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8))),
  list("Machine03",    98,             list(c(sensor03 = 11)))
))
# (Just to make clear what a `list` column is -- another way to write this would be the following:)
widget.corp.data.list <- data.table(
  machine = c("Machine01", "Machine02", "Machine03", "Machine03", "Machine03", "Machine02", "Machine01",
              "Machine01", "Machine02", "Machine01", "Machine03"),
  quality = c(78, 28, 32, 80, 58, 74, 46, 24, 7, 22, 98),
  sensor = list(
    c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23),
    c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27),
    c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10),
    c(sensor02 = 32.3),
    c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3),
    c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3),
    c(sensor01 = 81),
    c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22),
    c(sensor01 = 96, sensor02 = 96, sensor03 = 0),
    c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8),
    c(sensor03 = 11)
  )
)
# Be aware that some rows may not have any non-missing sensor data, in which case the `sensor` value for the
# corresponding result row should be an empty `numeric`.
ex01ListTable <- function(data) {
  # your code
  assertDataTable(data)

  data[, sensor := list(list(c(na.omit(unlist(.SD))))),
       .SDcols = patterns("^sensor[0-9]+"), by = .I][,
                                                     c("machine", "quality", "sensor"), with = FALSE
       ]
}

# You want to analyse the effect of measured sensor data on the quality of widgets, for each machine
# separately. However, you are worried that too much missing data will bring on misleading results.
# Therefore, you plan to remove columns with too much missing data.

# Write a function that removes all `sensorXX` columns which have
# 50% or more missing data for at least one machine. Your function should accept one `data.table` argument
# `data` and return the modified `data.table`. For the example `widget.corp.data` dataset,
# that would be `sensor01` (50% missing for "Machine03") and `sensor04` (75% missing for
# both "Machine01" and "Machine03"). The resulting dataset would therefore be
widget.corp.data.fsel <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor02 = NULL, sensor03 = NULL),
  list("Machine01",    78,             28.6,            -23),
  list("Machine02",    28,             77.8,            NA),
  list("Machine03",    32,             91.6,            -29),
  list("Machine03",    80,             32.3,            NA),
  list("Machine03",    58,             77.8,            3),
  list("Machine02",    74,             24.5,            -18),
  list("Machine01",    46,             NA,              NA),
  list("Machine01",    24,             13.3,            -22),
  list("Machine02",    7,              96.0,            0),
  list("Machine01",    22,             23.5,            7),
  list("Machine03",    98,             NA,              11)
))
ex02CleanTable <- function(data) {
  # your code
  assertDataTable(data)

  keep <- data[,
               as.list(colMeans(is.na(.SD))),
               by = "machine",
               .SDcols = patterns("^sensor[0-9]+")
  ][, machine := NULL
  ][, colnames(.SD)[vapply(.SD, max, numeric(1)) < 0.5]
  ]
  data[, c("machine", "quality", keep), with = FALSE]
}
