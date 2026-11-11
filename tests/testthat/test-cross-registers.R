testthat::test_that("test-cross-registers.R", {
  testthat::expect_no_error({
    library(eulob)

    # 1. Coincidencias presentes en dos o más países.
    all <- eulob_cross_registers()

    stopifnot(
      is.data.frame(all),
      nrow(all) > 0L,
      all(c("name_id", "country") %in% names(all))
    )

    countries_per_name <- tapply(
      all$country,
      all$name_id,
      function(x) length(unique(x))
    )

    stopifnot(all(countries_per_name >= 2L))

    # 2. Filtrado de coincidencias en tres o más países.
    three <- eulob_cross_registers(min_countries = 3)

    stopifnot(
      is.data.frame(three),
      identical(names(three), names(all)),
      all(three$name_id %in% all$name_id)
    )

    if (nrow(three) > 0L) {
      counts_three <- tapply(
        three$country,
        three$name_id,
        function(x) length(unique(x))
      )

      stopifnot(all(counts_three >= 3L))
    }

    # 3. Buscar una organización concreta.
    g <- eulob_cross_registers("Grünenthal GmbH")

    stopifnot(
      is.data.frame(g),
      identical(names(g), names(all)),
      nrow(g) > 0L,
      length(unique(g$country)) >= 2L
    )

    # 4. Comprobar el límite de organizaciones.
    limited <- eulob_cross_registers(limit = 10)

    stopifnot(
      is.data.frame(limited),
      nrow(limited) > 0L,
      length(unique(limited$name_id)) <= 10L,
      all(limited$name_id %in% all$name_id)
    )

    # 5. Una búsqueda inexistente debe estar vacía.
    empty <- eulob_cross_registers(
      "nonexistent_name_xyz_123"
    )

    stopifnot(
      is.data.frame(empty),
      nrow(empty) == 0L,
      identical(names(empty), names(all))
    )

    cat(
      "OK: cross-registers con catálogo ampliado\n",
      "Organizaciones compartidas: ",
      length(unique(all$name_id)), "\n",
      "Relaciones compartidas: ", nrow(all), "\n",
      "Organizaciones en >=3 países: ",
      length(unique(three$name_id)), "\n",
      sep = ""
    )
  })
})
