testthat::test_that("test-info.R", {
  testthat::expect_no_error({
    library(eulob)

    info <- eulob_info()

    stopifnot(
      is.list(info),
      identical(info$project, "EU-LOB"),
      identical(info$package, "eulob"),
      identical(
        info$version,
        as.character(utils::packageVersion("eulob"))
      )
    )

    cat("TEST INFO: OK\n")
  })
})

testthat::test_that("test-version.R", {
  testthat::expect_no_error({
    library(eulob)

    version <- eulob_version()

    stopifnot(
      inherits(version, "package_version"),
      as.character(version) ==
        as.character(utils::packageVersion("eulob"))
    )

    cat("TEST VERSION: OK\n")
  })
})
