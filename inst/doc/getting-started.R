## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(eulob)
eulob_version()

## -----------------------------------------------------------------------------
head(eulob_countries())
head(eulob_registers())

## -----------------------------------------------------------------------------
eulob_registers(country = "IT")
eulob_sources(country = "ES")

## -----------------------------------------------------------------------------
spanish <- eulob_lobbies(country = "ES")
head(spanish, 5)

## -----------------------------------------------------------------------------
if (nrow(spanish) > 0L) {
  entry <- spanish[1L, ]
  head(eulob_get(
    country = entry$country,
    lobby_id = entry$lobby_id,
    register = entry$register
  ))
}

## -----------------------------------------------------------------------------
head(eulob_search("association", limit = 5))
head(eulob_search("universidad", country = "ES", limit = 5))

## -----------------------------------------------------------------------------
head(eulob_search(
  "universidad",
  method = "fixed",
  limit = 5
))

## -----------------------------------------------------------------------------
head(eulob_search_normalized(
  "association",
  method = "strict",
  limit = 5
))

## -----------------------------------------------------------------------------
head(eulob_cross_registers(
  min_countries = 2L,
  limit = 5L
))

## -----------------------------------------------------------------------------
eulob_info()
eulob_version()

