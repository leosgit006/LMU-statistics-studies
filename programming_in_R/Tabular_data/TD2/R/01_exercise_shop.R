# In the following tasks you should write functions that handle data in a
# data.table. For all "data.table" inputs, your functions should
# `assertDataTable` the input value, but do not need to make any further
# assertions regarding the format of `data.table` arguments: You may assume
# that all "data.table" inputs have the correct columns in the correct format
# etc.

# Hint:
# For all these functions, you are given example input data that you can work
# with. Note that these are just *exapmles*, the tests will call your functions
# with other tables as well to make sure that your functions work in general,
# not just with the examples. Example tables often have the format
# tablename <- rbindlist(list(
#   list(title1 = NULL, title2 = NULL, ....),
#   list(value,       , value,         ....),
#   ....
# ))
# which you can run in the R console (in RStudio, just press ctrl-return) to get
# the variable `tablename`.
#
# Important:
# You may be writing some functions that modify the input data in-place. This
# means that calling one of your functions may actually cause the input value to
# be different after the call. Be aware of this if you try out your functions on
# the example data. E.g. if you do
# > result <- exXXFunction(input)
# then `input` itself may have changed and could give different results in
# future experiments, depending on what your code does with it.You should
# therefore always run your functions on a `copy` of the input data, like so:
# > result <- exXXFunction(copy(input))
# Alternatively, you can just execute the code that creates the example input
# data (or source this .R-file) again after each experiment.

# This exercise concerns itself with data in a certain format.
#
# A friend asked you to help him with his small corner store for various
# items. Because his shop is both a physical store where customers can
# take items, as well as an online shop that ships items, he has separate
# prices for items with and without delivery to reflect the different
# realities of the two markets.
# Your friend changes his inventory and prices monthly, and keeps a record of all
# sales made in that month. He wants to have a few functions that summarize
# the sales and revenue made in a month.
# His price-list could, for example, look like the following:
itemshop.prices <- rbindlist(list(
  list(item = NULL,             price.onsite = NULL, price.online = NULL),
  list("Healing Potion",        9.99,                12.99),
  list("Staff of Illusion",     18.95,               20.00),
  list("Lesser Stone of Mana",  2.60,                4.00),
  list("Greater Stone of Mana", 7.50,                9.99),
  list("Sword of Clarity +2",   21.50,               22.99)
))
# (the columns are constant, but actual datasets may have more or fewer rows
# with different items).
# Furthermore, your friend keeps a record of items sold in a table.
# The sales record has the following format:
itemshop.sales <- rbindlist(list(
  list(item = NULL,             channel = NULL),
  list("Healing Potion",        "online"),
  list("Sword of Clarity +2",   "onsite"),
  list("Sword of Clarity +2",   "online"),
  list("Sword of Clarity +2",   "onsite"),
  list("Greater Stone of Mana", "onsite")
))
# (Again, actual datasets may have more or fewer rows.)
#
# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.


# Write a function that counts the number of items sold for each item type.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output should be a `data.table` with columns `item` and `count`. Items
# with no sales should appear with a count of 0. The items
# should be in the same order as they appear in the `prices` table. The
# output with the two example datasets would be
itemshop.salescount <- rbindlist(list(
  list(item = NULL,             count = NULL),
  list("Healing Potion",        1),
  list("Staff of Illusion",     0),
  list("Lesser Stone of Mana",  0),
  list("Greater Stone of Mana", 1),
  list("Sword of Clarity +2",   3)
))
# You can use aggregation with `[... by ...]` here, and the special value `.N`
# may be useful. To get the same order as the `prices` table, a join could
# be useful.
ex01CountSales <- function(prices, sales) {
  # your code
  assertDataTable(prices)
  assertDataTable(sales)

  sales[prices, on = "item"][,
                             .("count" = sum(!is.na(channel))), by = .(item)]
}

# Write a function that counts the number of items sold for each type, and
# for each sales channel.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output should be a `data.table` with columns `item`, `count.onsite`, and
# `count.online`. Items with no sales should appear with a count of 0.
# The items should be in the same order as they appear in the `prices` table.
# The output with the two example datasets would be
itemshop.saleschannel.count <- rbindlist(list(
  list(item = NULL,             count.onsite = NULL, count.online = NULL),
  list("Healing Potion",        0,                   1),
  list("Staff of Illusion",     0,                   0),
  list("Lesser Stone of Mana",  0,                   0),
  list("Greater Stone of Mana", 1,                   0),
  list("Sword of Clarity +2",   2,                   1)
))
# This can be solved with aggregation similar to ex01CountSales, but `?dcast()`
# (followed by a join) also works.
ex02CountSalesChannels <- function(sales, prices) {
  # your code
  assertDataTable(prices)
  assertDataTable(sales)

  sales[prices, on = "item"][,
                             .("count.onsite" = sum(channel == "onsite", na.rm = TRUE),
                               "count.online" = sum(channel == "online", na.rm = TRUE)),
                             by = .(item)]
}

# Write a function that calculates the total revenue received by the shop.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output: a `numeric(1)`, summing up the total revenue as given by the price
# of each sold item for the respective channel.
# For the example dataset, the result would be `86.48`.
ex03Revenue <- function(prices, sales) {
  assertDataTable(prices)
  assertDataTable(sales)

  # Preise per Join hinzufügen, dann je nach channel den richtigen Preis nehmen
  merged <- prices[sales, on = "item"]

  revenue <- merged[, ifelse(channel == "onsite", price.onsite, price.online)]

  sum(revenue)
}
