## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(eulob)
eulob_countries()
eulob_registers()

## -----------------------------------------------------------------------------
head(eulob_lobbies(country = "ES"), 5)
head(eulob_lobbies(country = "DE"), 5)

## -----------------------------------------------------------------------------
eulob_sources(country = "IT")

## -----------------------------------------------------------------------------
head(eulob_search("association", limit = 5))
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
eulob_version()
eulob_info()

