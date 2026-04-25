
# You are analysing the hold times of a national package delivery company.
#
# Customers deliver packages to one of many storage warehouses, where they are
# accepted and processed. A fleet of transport vehicles travels between the
# warehouses every day to bring packages either to the final recipient, or to
# other storage warehouses, should the recipient be too far away. Packages that
# are received by a storage warehouse are kept there at least until the next
# morning, but because of limited fleet capacity or other circumstances they may
# remain there for longer.
# You are given the record of package deliveries between customers (identified
# as `"CUSTOMER"`), storage warehouses (identified by unique string ID), and
# recipients (identified as `"RECIPIENT"`). Every package has a shipment ID,
# and the data of the transaction is recorded as the number of days since
# 1970-01-01. An example record could look like the following:
package.transactions <- rbindlist(list(
    list(shipment.id = NULL, from.id = NULL, to.id = NULL, date = NULL),
    list("2458B9",           "CUSTOMER",     "xknq",       17571),
    list("2458B9",           "xknq",         "hlcf",       17572),
    list("587FB8",           "CUSTOMER",     "hlcf",       17573),
    list("2458B9",           "hlcf",         "RECIPIENT",  17574),
    list("3B444F",           "CUSTOMER",     "hlcf",       17574),
    list("587FB8",           "hlcf",         "xknq",       17575),
    list("587FB8",           "xknq",         "hlcf",       17576),
    list("587FB8",           "hlcf",         "RECIPIENT",  17577),
    list("3B444F",           "hlcf",         "RECIPIENT",  17577)
))
# In this example, the "2458B9" package was received by "xknq" (first entry),
# forwarded to "hlcf" after one day (second entry), and delivered to the
# recipient after two more days (fourth entry).
# The "587FB8" package was received by "hlcf" (third line), forwarded to
# "xknq" after two days (6th line) and returned back to "hlcf" after one more
# day (7th line), possibly because the original recipient cancelled their order.
# The package was then finally delivered to a recipient from "hlcf" (8th line).
#
# Note that, for real data, many deliveries for many storage warehouses are
# conducted on the same day, but that each package always stays at a warehouse
# for at least one day. Some packages may go through some storage houses
# multiple times (since customers still have the option to update shipping
# information after giving up their package). The data will always be given
# sorted by the `date` column.
#
# Write a function that takes the record of package shipments as presented and
# returns a table of average hold times for each warehouse.
# Input: `transactions`, a `data.table` as shown above.
# Return: a `data.table`, with columns `warehouse` (the warehouse ID) and
# `mean.hold.time`, the mean hold time, in days. A return value for the data
# presented above could, for example, be:
package.holdtimes <- rbindlist(list(
    list(warehouse = NULL, mean.hold.time = NULL),
    list("hlcf",           2),
    list("xknq",           1)
))
# (This is because packages "2458B9" and "587FB8" both spend one day at "xknq".
# Package "2458B9" is at "hlcf" for 2 days, package "3B444F" is at "hlcf" for 3
# days, and "587FB8" is at "hlcf" twice: once for 2 days, then again for 1 day,
# making for a mean handling time of 2).
#
# As an intermediate result, you probably want to have a table that lists for
# each transaction "arrival" record / date the corresponding "departure" / date,
# from which you can then calculate the hold time and then do aggregation.
# One possible way to do this is to use a rolling join (on the table itself):
# https://www.gormanalysis.com/blog/r-data-table-rolling-joins/
# You can also have a look at 02_exercise_bank.R:ex03Balance first, which
# does rolling joins in a similar (but maybe more intuitive) way.
ex01PackageHoldTimes <- function(transactions) {
  assertDataTable(transactions)

  # Ankunfts-Ereignisse: from.id ist das Lager, das das Paket empfängt
  arrivals <- transactions[from.id != "RECIPIENT",
                           .(shipment.id, warehouse = to.id, arrive.date = date)]

  # Nur echte Lager (kein RECIPIENT als Ziel)
  arrivals <- arrivals[warehouse != "RECIPIENT"]

  # Abgangs-Ereignisse: from.id ist das Lager, das das Paket weitergibt
  departures <- transactions[from.id != "CUSTOMER",
                             .(shipment.id, warehouse = from.id, depart.date = date)]

  # Nur echte Lager (kein CUSTOMER als Quelle)
  departures <- departures[warehouse != "CUSTOMER"]

  # Setkey für Rolling Join
  setkey(arrivals,   shipment.id, warehouse, arrive.date)
  setkey(departures, shipment.id, warehouse, depart.date)

  # Rolling Join: für jede Ankunft das nächste Abgangsdatum finden
  joined <- departures[arrivals, roll = -Inf]
  # roll = -Inf: nächstes Departure >= arrive.date

  joined[, hold.time := depart.date - arrive.date]

  # Mittlere Haltezeit pro Lager
  result <- joined[, .(mean.hold.time = mean(hold.time)), by = warehouse]

  result
}
