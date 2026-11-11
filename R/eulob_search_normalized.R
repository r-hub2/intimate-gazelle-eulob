#' Search normalized names and candidate equivalences
#'
#' Searches the eulob catalogue using documented registration
#' aliases and a locally stored index of nominal candidates.
#'
#' Strict mode uses exact names, registration aliases and
#' conservative orthographic candidates. Exploratory mode
#' additionally uses fuzzy candidates and distinctive-name
#' prefixes. Neither method proves legal identity.
#'
#' @param query Single name to search.
#' @param method One of "strict", "exploratory" or "both".
#' @param limit Maximum number of returned name records.
#' @param min_similarity Minimum similarity for fuzzy
#'   candidates in exploratory mode. Default 0.88.
#'
#' @return Data frame with original registration identifiers,
#'   match types, scores and review indicators.
#' @export
eulob_search_normalized <- function(
  query,
  method = c("strict", "exploratory", "both"),
  limit = 100L,
  min_similarity = 0.88,
  candidate_filter = c("focused", "all")
) {

  method <- match.arg(method)
  candidate_filter <- match.arg(candidate_filter)

  if (
    !is.character(query) ||
    length(query) != 1L ||
    is.na(query) ||
    !nzchar(trimws(query))
  ) {
    stop("query must be one non-empty character string.")
  }

  if (
    !is.numeric(limit) ||
    length(limit) != 1L ||
    is.na(limit) ||
    !is.finite(limit) ||
    limit < 1 ||
    limit != floor(limit)
  ) {
    stop("limit must be a positive integer.")
  }

  if (
    !is.numeric(min_similarity) ||
    length(min_similarity) != 1L ||
    is.na(min_similarity) ||
    !is.finite(min_similarity) ||
    min_similarity < 0 ||
    min_similarity > 1
  ) {
    stop("min_similarity must be between 0 and 1.")
  }

  norm <- function(s) {
    s <- tolower(s)
    s <- gsub("[[:space:]]+", " ", s)
    trimws(s)
  }


  # V4: términos distintivos para priorizar candidatos.
  # Esta puntuación no representa probabilidad de identidad.

  v4_tokens <- function(z) {

    z <- tolower(z)
    z <- gsub("[[:punct:]]+", " ", z)
    z <- gsub("[[:space:]]+", " ", z)
    z <- trimws(z)

    if (!nzchar(z)) return(character())

    tok <- unique(
      strsplit(z, " ", fixed = TRUE)[[1L]]
    )

    generic <- c(
      "the", "of", "for", "and", "or",
      "et", "de", "des", "du", "la", "le", "les",
      "der", "die", "das", "den", "dem",
      "und", "f\u00fcr", "von", "am", "in", "im",
      "auf", "an", "ein", "eine", "au", "aux",
      "a", "e", "v", "co", "s", "p",
      "ev", "asbl", "gmbh", "ggmbh",
      "ag", "sa", "sas", "srl", "sl",
      "ltd", "limited", "inc",
      "oy", "oyj", "bv", "nv", "llc",
      "se", "spa", "company", "group",
      "european", "international",
      "association", "asociacion", "asociaci\u00f3n",
      "associazione", "verband", "bundesverband",
      "deutscher", "deutsche", "deutschen",
      "deutsches", "gesellschaft", "nationale",
      "national", "fran\u00e7ais", "fran\u00e7aise",
      "francais", "francaise",
      "federation", "f\u00e9d\u00e9ration",
      "foundation", "fundacion", "fundaci\u00f3n",
      "fondation", "stiftung", "union",
      "entreprise", "entreprises",
      "empresa", "empresas", "syndicat",
      "institut", "institute", "comite",
      "comit\u00e9", "polskie", "stowarzyszenie",
      "bundesvereinigung",
      "bundesarbeitsgemeinschaft",
      "societe", "soci\u00e9t\u00e9", "sociedad",
      "societ\u00e0", "deutsch", "europe",
      "france", "germany", "deutschland",
      "italy", "ireland", "finland",
      "belgium", "belgique"
    )

    tok[
      nchar(tok) > 1L &
      !(tok %in% generic)
    ]
  }

  v4_relevance <- function(a, b) {

    x <- v4_tokens(a)
    y <- v4_tokens(b)

    comunes <- intersect(x, y)
    todos <- union(x, y)

    if (!length(todos)) return(0)

    length(comunes) / length(todos)
  }

  # Reutilizar el catálogo y el orden original en caché.
  cache <- .eulob_catalog_cache
  cache_key <- "lobbies_dt_order"

  if (!exists(cache_key, envir = cache, inherits = FALSE)) {
    invisible(eulob_lobbies())
  }

  cached <- get(cache_key, envir = cache, inherits = FALSE)

  cols <- c("country", "register", "lobby_id", "name")

  datos <- cached$data[
    cached$order,
    cols,
    with = FALSE
  ]

  # Deduplicar manteniendo la primera aparición.
  datos <- unique(datos, by = cols)
  datos <- as.data.frame(datos)

  for (col in cols) {
    datos[[col]] <- as.character(datos[[col]])
  }

  datos <- datos[
    stats::complete.cases(datos) &
      nzchar(datos$name) &
      nzchar(datos$country) &
      nzchar(datos$register) &
      nzchar(datos$lobby_id),
    ,
    drop = FALSE
  ]

  datos$key <- norm(datos$name)

  datos$registration <- paste(
    datos$country,
    datos$register,
    datos$lobby_id,
    sep = "\034"
  )

  q <- norm(query)

  vacio <- data.frame(
    country = character(),
    register = character(),
    lobby_id = character(),
    name = character(),
    matched_name = character(),
    match_type = character(),
    similarity = numeric(),
    evidence = character(),
    match_level = character(),
    requires_match_review = logical(),
    identity_verified = logical(),
    candidate_relevance = numeric(),
    candidate_relation = character(),
    candidate_filter = character(),
    stringsAsFactors = FALSE
  )

  # Step 1: Exact nominal matches and documented names
  # associated with the same registration.

  direct <- datos[datos$key == q, , drop = FALSE]

  seeds <- q

  if (nrow(direct)) {
    keys <- unique(direct$registration)

    seeds <- unique(c(
      q,
      datos$key[datos$registration %in% keys]
    ))
  }

  coincidencias <- data.frame(
    key = q,
    match_type = "normalized_exact",
    similarity = 1,
    match_level = "strict",
    evidence = "normalized_name_equality",
    requires_match_review = FALSE,
    stringsAsFactors = FALSE
  )

  alias <- setdiff(seeds, q)

  if (length(alias)) {
    coincidencias <- rbind(
      coincidencias,
      data.frame(
        key = alias,
        match_type = "documented_registration_alias",
        similarity = 1,
        match_level = "strict",
        evidence = "shared_registration_identifier",
        requires_match_review = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }

  # Step 2: Precomputed V2/V3 candidates.

  ruta <- system.file(
    "extdata",
    "catalog", "eulob_normalized_candidates.rds",
    package = "eulob"
  )

  if (!nzchar(ruta)) {
    stop("The normalized candidate index is missing.")
  }

  cache_key <- "normalized_candidates"

  if (!exists(cache_key, envir = .eulob_catalog_cache,
              inherits = FALSE)) {
    assign(
      cache_key,
      readRDS(ruta),
      envir = .eulob_catalog_cache
    )
  }

  indice <- get(
    cache_key,
    envir = .eulob_catalog_cache,
    inherits = FALSE
  )

  encontrados <- indice[
    indice$nombre_1 == q |
      indice$nombre_2 == q,
    ,
    drop = FALSE
  ]

  if (nrow(encontrados)) {

    candidato <- ifelse(
      encontrados$nombre_1 == q,
      encontrados$nombre_2,
      encontrados$nombre_1
    )

    sim <- 1 - encontrados$distancia_jw

    conservar <- if (method == "strict") {
      encontrados$strict_candidate
    } else {
      encontrados$strict_candidate |
        sim >= min_similarity
    }

    # For short queries, fuzzy candidates must preserve
    # the complete query as the first name token.
    # This prevents OpenAI -> OPEN! or OPENS.
    # Documented aliases are returned but never used
    # as additional fuzzy expansion seeds.

    if (nchar(q) <= 6L) {
      conservar <- conservar &
        startsWith(candidato, paste0(q, " "))
    }

    encontrados <- encontrados[conservar, , drop = FALSE]
    candidato <- candidato[conservar]
    sim <- sim[conservar]

    if (nrow(encontrados)) {

      seguro <- encontrados$strict_candidate

      coincidencias <- rbind(
        coincidencias,
        data.frame(
          key = candidato,
          match_type = ifelse(
            seguro,
            "conservative_orthographic_candidate",
            "fuzzy_candidate"
          ),
          similarity = sim,
          match_level = ifelse(
            seguro,
            "strict",
            "exploratory"
          ),
          evidence = ifelse(
            seguro,
            "conservative_v2_v3_name_rule",
            "v2_v3_fuzzy_similarity"
          ),
          requires_match_review = TRUE,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # Step 3: Exploratory extension for distinctive short
  # names: Google -> Google France, etc.
  # This is a nominal lead, not proof of common legal identity.

  if (method != "strict") {

    prohibidos <- c(
      "association", "asociacion", "asociaci\u00f3n",
      "european", "international", "federation",
      "f\u00e9d\u00e9ration", "bundesverband", "verband",
      "stiftung", "company", "the", "union",
      "deutsche", "deutscher", "eesti"
    )

    una_palabra <- !grepl(" ", q, fixed = TRUE)

    if (
      una_palabra &&
      nchar(q) >= 5L &&
      !(q %in% prohibidos)
    ) {

      candidatos_prefijo <- unique(
        datos$key[
          startsWith(datos$key, paste0(q, " "))
        ]
      )

      if (length(candidatos_prefijo)) {
        coincidencias <- rbind(
          coincidencias,
          data.frame(
            key = candidatos_prefijo,
            match_type = "shared_name_prefix_candidate",
            similarity = NA_real_,
            match_level = "exploratory",
            evidence = "shared_distinctive_name_prefix",
            requires_match_review = TRUE,
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }

  # Step 4: Prioritize direct evidence over fuzzy leads.
  # No transitive merging of candidate relationships.

  prioridad <- match(
    coincidencias$match_type,
    c(
      "normalized_exact",
      "documented_registration_alias",
      "conservative_orthographic_candidate",
      "fuzzy_candidate",
      "shared_name_prefix_candidate"
    )
  )

  similitud_orden <- coincidencias$similarity
  similitud_orden[is.na(similitud_orden)] <- -1

  coincidencias <- coincidencias[
    order(
      prioridad,
      -similitud_orden,
      coincidencias$key
    ),
    ,
    drop = FALSE
  ]

  coincidencias <- coincidencias[
    !duplicated(coincidencias$key),
    ,
    drop = FALSE
  ]

  # Match to original catalogue records.

  posiciones <- match(datos$key, coincidencias$key)

  selected <- datos[
    !is.na(posiciones),
    ,
    drop = FALSE
  ]

  posiciones <- posiciones[!is.na(posiciones)]

  if (!nrow(selected)) {
    return(vacio)
  }

  selected$matched_name <- coincidencias$key[posiciones]
  selected$match_type <- coincidencias$match_type[posiciones]
  selected$similarity <- coincidencias$similarity[posiciones]
  selected$evidence <- coincidencias$evidence[posiciones]
  selected$match_level <- coincidencias$match_level[posiciones]
  selected$requires_match_review <-
    coincidencias$requires_match_review[posiciones]


  selected$identity_verified <- FALSE

  # V4: puntuación de relevancia léxica.
  # No certifica equivalencia ni identidad jurídica.

  selected$candidate_relevance <- vapply(
    selected$name,
    function(candidate) v4_relevance(q, candidate),
    numeric(1)
  )

  supported <- selected$match_type %in% c(
    "normalized_exact",
    "documented_registration_alias"
  )

  orthographic <- selected$match_type ==
    "conservative_orthographic_candidate"

  prefix <- selected$match_type ==
    "shared_name_prefix_candidate"

  fuzzy <- selected$match_type ==
    "fuzzy_candidate"

  selected$candidate_relation <- ifelse(
    supported,
    "registration_or_exact_name",
    ifelse(
      orthographic,
      "unverified_orthographic_candidate",
      ifelse(
        prefix,
        "possible_related_organization",
        ifelse(
          selected$candidate_relevance >= 0.5,
          "shared_distinctive_terms",
          "low_information_fuzzy"
        )
      )
    )
  )

  selected$candidate_filter <- candidate_filter

  # focused: descarta candidatos fuzzy débiles.
  # all: conserva todos los candidatos exploratorios.
  # Los alias documentados nunca se descartan.

  if (
    candidate_filter == "focused" &&
    method != "strict"
  ) {

    keep <- !fuzzy |
      selected$candidate_relevance >= 0.5

    selected <- selected[
      keep,
      ,
      drop = FALSE
    ]
  }


  rank <- match(
    selected$match_type,
    c(
      "normalized_exact",
      "documented_registration_alias",
      "conservative_orthographic_candidate",
      "fuzzy_candidate",
      "shared_name_prefix_candidate"
    )
  )

  selected <- selected[
    order(
      rank,
      -selected$candidate_relevance,
      selected$country,
      selected$register,
      selected$lobby_id,
      selected$name
    ),
    ,
    drop = FALSE
  ]

  selected <- head(selected, limit)

  result <- selected[, names(vacio), drop = FALSE]
  rownames(result) <- NULL

  result
}
