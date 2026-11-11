testthat::test_that("test-cache.R", {
  testthat::expect_no_error({
    library(eulob)

    # Internal cache should exist without requiring extra packages.
    ns <- asNamespace("eulob")

    cache <- get(
      ".eulob_catalog_cache",
      envir = ns,
      inherits = FALSE
    )

    catalog <- get(
      ".eulob_catalog",
      envir = ns,
      inherits = FALSE
    )

    stopifnot(is.environment(cache))

    # Both bundled tables must be available and reusable.
    for (table in c("names", "name_ids")) {

      first <- catalog(table)
      second <- catalog(table)

      stopifnot(
        is.data.frame(first),
        identical(first, second),
        exists(table, envir = cache, inherits = FALSE),
        identical(
          first,
          get(table, envir = cache, inherits = FALSE)
        )
      )

      # Verify that the cached object matches the original file.
      path <- system.file(
        "extdata",
        "catalog",
        paste0(table, ".rds"),
        package = "eulob"
      )

      stopifnot(
        nzchar(path),
        identical(first, readRDS(path))
      )
    }

    # Public API must remain usable after cache initialization.
    stopifnot(
      is.data.frame(eulob_countries()),
      is.data.frame(eulob_registers()),
      is.data.frame(eulob_lobbies()),
      is.data.frame(
        eulob_search("Siemens", method = "fixed", limit = 5L)
      )
    )

    # Memory retained by the two catalogue tables.
    cache_mb <- sum(vapply(
      ls(cache),
      function(table) {
        as.numeric(object.size(
          get(table, envir = cache, inherits = FALSE)
        ))
      },
      numeric(1)
    )) / 1024^2

    cat(
      sprintf(
        "Cached catalogue memory: %.2f MB\n",
        cache_mb
      )
    )

    cat("Catalogue cache tests: OK\n")
  })
})
