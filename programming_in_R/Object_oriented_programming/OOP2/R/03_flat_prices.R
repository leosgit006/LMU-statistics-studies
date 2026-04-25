
# You are looking for a new flat. For this, you query a database for monthly
# rent prices of available flats, which returns data in the following format:
flat.prices <- rbindlist(list(
    list(address = NULL, prices = NULL),
    list("134 Charles St", list(list(c(2340, 2193), NULL, 4023, NULL, NULL, c(10234, 9203)))),
    list("12 East 69th St", list(list(2493, NULL, NULL, NULL))),
    list("2 Park Pl", list(list(NULL, NULL, 1924, 3921))),
    list("172 Madison Ave", list(list(NULL, NULL))),
    list("25 Columbus Circle", list(list(10234)))
))
# This lists the adresses where the agency manages flats, and for each address
# the column `prices` lists the flat prices. This column contains a list for
# each address, where the first entry lists all prices of all available flats
# on the first floor, the second entry lists all prices of available flats on
# the second floor etc. In this example, the `"134 Charles St"` building has
# two flats available on the ground floor (one for 2340, one for 2193), one flat
# for 4023 on the second floor, and two flats (10234 and 9203) on the fifth
# floor.
#
# You plan to look at all available addresses individually, but within each
# address you only consider the *cheapest flat that is not on the ground floor*.
# You therefore need to write a function that lists the address, the price of
# the cheapest apartment that is not on the ground floor, and the floor of that
# aparment. If there are no apartments available that are not on the ground
# floor, the address should be absent. The result for the input above should
# therefore be
flat.choices <- rbindlist(list(
    list(address = NULL, price = NULL, floor = NULL),
    list("134 Charles St", 4023, 2),
    list("2 Park Pl", 1924, 2)
))
# Input:
# - `prices`: a `data.table` with columns `address`, `prices`, as described
#   above
# Output:
# A `data.table with columns `address`, `price`, `floor`, as shown above,
# with arbitrary order of rows.
ex01FlatPrices <- function(prices) {
  assertDataTable(prices)

  results <- rbindlist(lapply(seq_len(nrow(prices)), function(i) {
    addr   <- prices$address[[i]]
    floors <- prices$prices[[i]]  # Liste von Etagen (Index 1 = EG)

    # Nur Etagen ab Index 2 (nicht Erdgeschoss)
    upper.floors <- floors[-1]

    if (length(upper.floors) == 0) return(NULL)

    # Pro Etage: günstigstes Angebot finden
    best <- rbindlist(lapply(seq_along(upper.floors), function(j) {
      flat.prices <- upper.floors[[j]]
      if (is.null(flat.prices) || length(flat.prices) == 0) return(NULL)
      data.table(price = min(unlist(flat.prices)), floor = j + 1)
    }))

    if (nrow(best) == 0) return(NULL)
    
    # Günstigste Etage über alle Obergeschosse
    best <- best[which.min(price)]
    data.table(address = addr, price = best$price, floor = best$floor)
  }))

  results
}
