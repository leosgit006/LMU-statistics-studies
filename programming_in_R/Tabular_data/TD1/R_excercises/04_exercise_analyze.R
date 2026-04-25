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

# Even after removing columns with 50% or more missing data for any machine, you are still left with
# a dataset with missing values. You don't want to remove all columns with missing data (which would,
# in most cases, leave you with no data at all), so instead you choose to "impute" missing values.
# You know that values are missing because they are below the detection threshold of a sensor.
# You don't know the detection thesholds, but you know that for each sensor they are the same on
# different machines. Therefore, you *estimate* the threshold as the *minimum non-missing value* for each sensor.
#
# Write a function that imputes all missing values for sensor columns as the minimum of that column.
# The function takes one `data.table` argument `data` and should return the modified `data.table`.
# You can rely on at least one non-missing value being present in each sensor-column. The result
# for the `widget.corp.data` example data would be
widget.corp.data.imputed <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
  list("Machine01",    78,             23,              28.6,            -23,             3),
  list("Machine02",    28,             41,              77.8,            -29,             27),
  list("Machine03",    32,             57,              91.6,            -29,             10),
  list("Machine03",    80,             10,              32.3,            -29,             3),
  list("Machine03",    58,             10,              77.8,            3,               3),
  list("Machine02",    74,             10,              24.5,            -18,             3),
  list("Machine01",    46,             81,              13.3,            -29,             3),
  list("Machine01",    24,             43,              13.3,            -22,             3),
  list("Machine02",    7,              96,              96.0,            0,               3),
  list("Machine01",    22,             107,             23.5,            7,               8),
  list("Machine03",    98,             10,              13.3,            11,              3)
))
ex01ImputeTable <- function(data) {
  # your code
  assertDataTable(data)

  sensors <- grep("^sensor", names(data), value = TRUE)
  for (s in sensors) {
    min.val <- min(data[[s]], na.rm = TRUE)
    setnafill(data, cols = s, fill = min.val)
  }
  return(data)
}

# You now have functions that get your data in a state fit for modelling, so you want to fit the actual models.
#
# For your analysis, you want to fit "linear models" using the `lm()` function that comes with `R`. If you
# had just a `data.table` of sensor data for a single machine, for example
widget.corp.small.example <- data.table(
  quality = c(78, 46, 24, 22),
  sensor02 = c(28.6, 13.3, 13.3, 23.5),
  sensor03 = c(-23, -29, -22, 7))
# then the linear model would be the result of calling `lm(quality ~ ., data = widget.corp.small.example)`.
# What is happening here is that the `quality ~ .` formula tells `lm()` to model the `quality`-column against
# all other values.
# You want to fit a linear model for each machine, and put the resulting linear models into a `data.table` for
# further analysis.
#
# Write a function that takes a `data.table` argument `data`. The function should use `ex02CleanTable()`
# from the previous exercise (03_exercise_clean.R) to remove columns with at least 50% missing values, and then
# `ex01ImputeTable()` to impute missing values in all remaining columns (This way you won't be fitting models on
# columns where most data is actually just imputed).
# The return value should be a `data.table` with columns `machine` (a `character` column, enumerating the machines
# in any order), and `model` (a `list` column, containing models). The models should be the result of `lm()`
# called on the data for each machine, as above.
# The result when called with the `widget.corp.data` example dataset could be
widget.corp.data.models <- data.table(
  machine = c("Machine01", "Machine02", "Machine03"),
  model = list(
    lm(quality ~ ., data.table(
      quality = c(78, 46, 24, 22),
      sensor02 = c(28.6, 13.3, 13.3, 23.5),
      sensor03 = c(-23, -29, -22, 7))),
    lm(quality ~ ., data.table(
      quality = c(28, 74, 7),
      sensor02 = c(77.8, 24.5, 96),
      sensor03 = c(-29, -18, 0))),
    lm(quality ~ ., data.table(
      quality = c(32, 80, 58, 98),
      sensor02 = c(91.6, 32.3, 77.8, 13.3),
      sensor03 = c(-29, -29, 3, 11)))
  )
)
# (but the ordering of rows does not matter)
# You might want to source the previous exercise file to make ex02CleanTable
# available.
ex02ModelTable <- function(data) {
  assertDataTable(data)

  # Bereinigen und imputieren
  cleaned <- ex02CleanTable(data)
  imputed <- ex01ImputeTable(cleaned)

  # Pro Maschine ein lm() fitten
  machines <- unique(imputed$machine)

  models <- lapply(machines, function(m) {
    machine.data <- imputed[machine == m, !"machine"]
    lm(quality ~ ., data = machine.data)
  })

  data.table(machine = machines, model = models)
}
