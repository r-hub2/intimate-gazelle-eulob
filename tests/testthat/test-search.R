testthat::test_that("test-search-cosine-new-countries.R", {
  testthat::expect_no_error({
    library(eulob)

    # Catálogo europeo ampliado: 19 ámbitos con entradas.
    countries <- eulob_countries()

    stopifnot(
      nrow(countries) == 19L,
      sum(countries$n_entities) == 42265L
    )

    # Prueba real del algoritmo de coseno implementado por eulob.
    results <- eulob_search(
      query = "Grayling",
      method = "cosine",
      q = 2L,
      limit = 30L
    )

    stopifnot(
      is.data.frame(results),
      nrow(results) > 0L,
      all(c("name", "country", "similarity") %in% names(results)),
      all(is.finite(results$similarity)),
      all(results$similarity >= 0 & results$similarity <= 1),
      all(diff(results$similarity) <= 1e-10)
    )

    # Resultado exacto conocido del registro europeo.
    stopifnot(
      any(
        results$name == "Grayling" &
        results$country == "UE" &
        abs(results$similarity - 1) < 1e-10
      )
    )

    # La composición del top 30 global cambia al incorporar
    # nuevas entidades. No exigir países concretos en ese ranking.
    # Las coincidencias conocidas se verifican por separado,
    # mediante búsquedas restringidas a cada país.

    # Verificar búsquedas exactas sobre nombres realmente indexados.
    # No depender de denominaciones que puedan cambiar entre capturas.
    for (country in c("FR", "CZ", "PL", "UK")) {
      entities <- eulob_lobbies(country = country)

      stopifnot(nrow(entities) > 0L)

      query <- entities$name[[1L]]

      x <- eulob_search(
        query = query,
        country = country,
        method = "cosine",
        q = 2L,
        limit = 5L
      )

      stopifnot(
        nrow(x) >= 1L,
        all(x$country == country),
        any(abs(x$similarity - 1) < 1e-10)
      )
    }

    cat("OK: búsqueda por coseno con nuevos países\n")
  })
})

testthat::test_that("test-search-methods.R", {
  testthat::expect_no_error({
    library(eulob)

    # 1. Buscar una entidad conocida del catálogo ampliado.
    # Verificamos comportamiento, no una posición histórica fija.

    for (method in c("cosine", "jaccard", "jw", "dl")) {

      x <- eulob_search(
        query = "Grayling",
        method = method,
        limit = 10L
      )

      stopifnot(
        is.data.frame(x),
        nrow(x) > 0L,
        nrow(x) <= 10L,
        all(c("name", "country", "similarity") %in% names(x)),
        all(is.finite(x$similarity)),
        all(x$similarity >= 0 & x$similarity <= 1),
        all(diff(x$similarity) <= 1e-10)
      )

      cat("OK:", method, "-", nrow(x), "resultados\n")
    }

    # 2. Comprobar el método por defecto: coseno con bigramas.

    default <- eulob_search("Grayling", limit = 10L)

    cosine <- eulob_search(
      "Grayling",
      method = "cosine",
      q = 2L,
      limit = 10L
    )

    stopifnot(identical(default, cosine))

    # 3. Una coincidencia exacta debe alcanzar similitud 1.

    exact <- eulob_search(
      "Grayling",
      method = "cosine",
      limit = 10L
    )

    stopifnot(
      any(
        exact$name == "Grayling" &
        abs(exact$similarity - 1) < 1e-10
      )
    )

    # 4. El filtro de país debe respetarse.

    fr <- eulob_search(
      "Grayling",
      country = "FR",
      method = "cosine",
      limit = 10L
    )

    stopifnot(
      nrow(fr) > 0L,
      all(fr$country == "FR")
    )

    # 5. El umbral mínimo debe respetarse.

    filtered <- eulob_search(
      "Grayling",
      method = "cosine",
      min_similarity = 0.8,
      limit = 20L
    )

    stopifnot(
      all(filtered$similarity >= 0.8)
    )

    # 6. El método fixed debe mantener búsquedas literales.

    fixed <- eulob_search(
      "Grayling",
      method = "fixed",
      limit = 10L
    )

    stopifnot(
      is.data.frame(fixed),
      nrow(fixed) > 0L,
      nrow(fixed) <= 10L,
      all(grepl("Grayling", fixed$name, ignore.case = TRUE))
    )

    # 7. Una búsqueda fija inexistente devuelve cero resultados.

    empty <- eulob_search(
      "nonexistent_name_xyz_123",
      method = "fixed"
    )

    stopifnot(
      is.data.frame(empty),
      nrow(empty) == 0L
    )

    cat("\nOK: todos los métodos de búsqueda\n")
  })
})

testthat::test_that("test-search-placeholders.R", {
  testthat::expect_no_error({
    library(eulob)

    markers <- c("[ND] [ND]", "N/A")

    # The original records must remain accessible.
    all <- eulob_lobbies()

    stopifnot(nrow(all) == 42599L)

    found <- all[
      all$name %in% markers,
      ,
      drop = FALSE
    ]

    stopifnot(
      nrow(found) == 7L,
      sum(found$name == "[ND] [ND]") == 6L,
      sum(found$name == "N/A") == 1L
    )

    # No placeholder may be returned as a search result.
    for (method in c("fixed", "cosine", "jaccard", "jw", "dl")) {

      for (query in c("[ND]", "N/A", "Grayling")) {

        result <- eulob_search(
          query,
          method = method,
          limit = 100L
        )

        stopifnot(
          is.data.frame(result),
          !any(result$name %in% markers)
        )
      }
    }

    # Searching the exact marker still must not return that marker.
    for (marker in markers) {

      result <- eulob_search(
        marker,
        method = "cosine",
        limit = 100L
      )

      stopifnot(!any(result$name == marker))
    }

    # A legitimate record must remain searchable after filtering.
    legitimate <- eulob_search(
      "University Medical Center Groningen",
      method = "fixed",
      country = "UE",
      limit = 100L
    )

    stopifnot(
      nrow(legitimate) > 0L,
      any(grepl(
        "University Medical Center Groningen",
        legitimate$name,
        fixed = TRUE
      )),
      !any(legitimate$name %in% markers)
    )

    # Cross-register lookup must exclude missing-name markers,
    # including when the minimum number of countries is one.
    for (marker in markers) {

      result <- eulob_cross_registers(
        name = marker,
        min_countries = 1L
      )

      stopifnot(nrow(result) == 0L)
    }

    shared <- eulob_cross_registers(min_countries = 1L)

    stopifnot(
      !any(shared$name %in% markers)
    )

    cat("OK: marcadores excluidos sin alterar datos originales\n")
  })
})

testthat::test_that("test-search-unicode.R", {
  testthat::expect_no_error({
    library(eulob)

    # Unicode normalization must match the catalog index.
    pairs <- list(
      c("Straße", "Strasse"),
      c("Grayling", "GRAYLING"),
      c("Straße", "STRASSE")
    )

    for (pair in pairs) {
      for (method in c("fixed", "cosine", "jaccard", "jw", "dl")) {
        a <- eulob_search(
          pair[1], method = method, limit = 20L
        )
        b <- eulob_search(
          pair[2], method = method, limit = 20L
        )

        stopifnot(
          identical(a$name_id, b$name_id),
          identical(a$country, b$country)
        )

        if ("similarity" %in% names(a)) {
          stopifnot(
            isTRUE(all.equal(
              a$similarity, b$similarity,
              tolerance = 1e-12
            ))
          )
        }
      }
    }

    # Canonically equivalent Unicode spellings.
    composed <- "\u00c9lectricit\u00e9"
    decomposed <- "E\u0301lectricite\u0301"

    a <- eulob_search(composed, method = "cosine", limit = 10L)
    b <- eulob_search(decomposed, method = "cosine", limit = 10L)

    stopifnot(
      identical(a$name_id, b$name_id),
      isTRUE(all.equal(a$similarity, b$similarity))
    )

    cat("OK: normalización Unicode coherente\n")
  })
})
