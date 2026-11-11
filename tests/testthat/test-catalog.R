testthat::test_that("test-catalog.R", {
  testthat::expect_no_error({
    library(eulob)

    # ============================================================
    # EU-LOB: pruebas de integridad del catálogo
    #
    # Las pruebas deben seguir siendo válidas cuando se incorporen
    # nuevas entidades, países, registros y denominaciones.
    # ============================================================

    cat("========== 1. CARGA DEL CATÁLOGO ==========\n")

    countries <- eulob_countries()
    registers <- eulob_registers()
    all <- eulob_lobbies()

    stopifnot(
      is.data.frame(countries),
      is.data.frame(registers),
      is.data.frame(all),
      nrow(countries) > 0L,
      nrow(registers) > 0L,
      nrow(all) > 0L
    )

    required_countries <- c("country", "n_entities")
    required_entities <- c(
      "country", "register", "lobby_id", "name_id", "name"
    )

    stopifnot(
      all(required_countries %in% names(countries)),
      all(required_entities %in% names(all))
    )

    cat("Países:", nrow(countries), "\n")
    cat("Registros:", nrow(registers), "\n")
    cat("Relaciones:", nrow(all), "\n")


    # ============================================================
    # 2. Integridad de países
    # ============================================================

    cat("\n========== 2. PAÍSES ==========\n")

    stopifnot(
      !anyNA(countries$country),
      !anyDuplicated(countries$country),
      all(nzchar(countries$country)),
      all(!is.na(countries$n_entities)),
      all(countries$n_entities > 0)
    )

    # Reconstruir los recuentos desde las entidades reales.
    # Una misma entidad puede tener varias denominaciones.
    entity_keys <- c("country", "register", "lobby_id")

    entities <- unique(
      all[, entity_keys, drop = FALSE]
    )

    actual_counts <- table(entities$country)

    expected <- setNames(
      as.numeric(countries$n_entities),
      countries$country
    )

    stopifnot(
      setequal(names(actual_counts), names(expected)),
      identical(
        as.numeric(actual_counts[names(expected)]),
        unname(expected)
      ),
      sum(countries$n_entities) == nrow(entities)
    )

    cat("Entidades únicas:", nrow(entities), "\n")
    cat("Recuentos por país: correctos\n")


    # ============================================================
    # 3. Integridad de los registros
    # ============================================================

    cat("\n========== 3. REGISTROS ==========\n")

    stopifnot(
      all(countries$country %in% registers$country),
      !anyNA(registers$country),
      all(registers$country %in% countries$country)
    )

    for (country in countries$country) {

      country_registers <- eulob_registers(country)
      country_entities <- eulob_lobbies(country = country)

      stopifnot(
        is.data.frame(country_registers),
        is.data.frame(country_entities),
        nrow(country_registers) > 0L,
        nrow(country_entities) > 0L,
        all(country_registers$country == country),
        all(country_entities$country == country)
      )

      # La consulta por país debe devolver exactamente las
      # relaciones que figuran en el catálogo global.
      expected_rows <- sum(all$country == country)

      stopifnot(
        nrow(country_entities) == expected_rows
      )

      # Cada entidad indexada debe pertenecer a un registro
      # declarado en el catálogo.
      if ("register" %in% names(country_registers)) {
        stopifnot(
          all(
            unique(country_entities$register) %in%
              country_registers$register
          )
        )
      }
    }

    cat("Todos los países y registros: correctos\n")


    # ============================================================
    # 4. Integridad de entidades y denominaciones
    # ============================================================

    cat("\n========== 4. DENOMINACIONES ==========\n")

    stopifnot(
      !anyNA(all$country),
      !anyNA(all$register),
      !anyNA(all$lobby_id),
      !anyNA(all$name_id),
      !anyNA(all$name),
      all(nzchar(all$country)),
      all(nzchar(all$register)),
      all(nzchar(all$lobby_id)),
      all(nzchar(all$name_id)),
      all(nzchar(all$name))
    )

    # Una entidad puede tener varias denominaciones, pero no
    # debe repetirse la misma relación nombre-entidad.
    relation_keys <- c(
      "name_id", "country", "register", "lobby_id"
    )

    stopifnot(
      !anyDuplicated(all[, relation_keys, drop = FALSE])
    )

    # Todas las entidades deben tener al menos una denominación.
    stopifnot(
      nrow(entities) > 0L,
      nrow(all) >= nrow(entities)
    )

    # Comprobar coherencia de los IDs normalizados.
    if ("name_normalized" %in% names(all)) {
      mapping <- unique(
        all[, c("name_id", "name_normalized"), drop = FALSE]
      )

      stopifnot(
        !anyDuplicated(mapping$name_id),
        !anyDuplicated(mapping$name_normalized)
      )
    }

    cat("Entidades únicas:", nrow(entities), "\n")
    cat("Relaciones nombre-entidad:", nrow(all), "\n")
    cat("Nombres únicos:", length(unique(all$name_id)), "\n")
    cat("Denominaciones: correctas\n")


    # ============================================================
    # 5. Consistencia de Alemania y Unión Europea
    # ============================================================

    cat("\n========== 5. DE Y UE ==========\n")

    for (country in intersect(c("DE", "UE"), countries$country)) {

      x <- eulob_lobbies(country = country)

      stopifnot(
        nrow(x) > 0L,
        all(x$country == country),
        nrow(x) == sum(all$country == country)
      )

      cat(country, ":", nrow(x), "relaciones\n")
    }


    # ============================================================
    # 6. Búsqueda básica
    # ============================================================

    cat("\n========== 6. BÚSQUEDA ==========\n")

    results <- eulob_search(
      query = "European",
      limit = 10L
    )

    stopifnot(
      is.data.frame(results),
      nrow(results) > 0L,
      nrow(results) <= 10L,
      all(c("name", "country") %in% names(results)),
      all(results$country %in% countries$country)
    )

    cat("Búsqueda básica: correcta\n")


    # ============================================================
    # 7. Resultado
    # ============================================================

    cat("\n==========================================\n")
    cat("TODAS LAS PRUEBAS DEL CATÁLOGO: OK\n")
    cat("Países:", nrow(countries), "\n")
    cat("Registros:", nrow(registers), "\n")
    cat("Entidades:", nrow(entities), "\n")
    cat("Relaciones:", nrow(all), "\n")
    cat("Nombres únicos:", length(unique(all$name_id)), "\n")
    cat("==========================================\n")
  })
})

testthat::test_that("eulob_sources", {
  registers <- eulob_registers()
  sources <- eulob_sources()

  keys <- c(
    "country", "register", "n_names", "n_entities"
  )

  testthat::expect_equal(
    sources[keys],
    registers[keys]
  )

  testthat::expect_false(
    anyDuplicated(sources[c("country", "register")]) > 0L
  )

  testthat::expect_equal(
    nrow(eulob_sources(country = "IT")),
    nrow(eulob_registers(country = "IT"))
  )
})
