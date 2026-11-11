# Source-specific missing-name markers.
# Preserve these records in the underlying catalogues.
.eulob_is_placeholder_name <- function(x) {
  !is.na(x) & trimws(x) %in% c("[nd] [nd]", "n/a")
}

# Normalize queries using the same Unicode rules as the catalog.
.eulob_normalize_query <- function(x) {
  x <- stringi::stri_trans_nfkc(x)
  x <- stringi::stri_trans_casefold(x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

# Internal: load the catalogue bundled with the package.
# Cache bundled catalogues lazily within the R session.
.datatable.aware <- TRUE

utils::globalVariables(c(".SD", "name_id", "lobby_id"))

.eulob_catalog_cache <- new.env(parent = emptyenv())

.eulob_catalog <- function(table, data_dir = NULL) {
  stopifnot(table %in% c("names", "name_ids"))

  if (!is.null(data_dir)) {
    stop(
      "data_dir is not supported by the bundled RDS catalogue. ",
      "Omit data_dir to use the installed catalogue.",
      call. = FALSE
    )
  }

  path <- system.file(
    "extdata", "catalog", paste0(table, ".rds"),
    package = "eulob"
  )

  if (!nzchar(path)) {
    stop("EU-LOB catalogue file not found: ", table,
         call. = FALSE)
  }

  if (!exists(table, envir = .eulob_catalog_cache,
              inherits = FALSE)) {
    assign(
      table,
      readRDS(path),
      envir = .eulob_catalog_cache
    )
  }

  get(
    table,
    envir = .eulob_catalog_cache,
    inherits = FALSE
  )
}

# Internal: validate optional country.
.eulob_country <- function(country) {
  if (is.null(country)) return(NULL)

  if (!is.character(country) ||
      length(country) != 1L ||
      is.na(country) ||
      !nzchar(trimws(country))) {
    stop("country must be a non-empty character string.",
         call. = FALSE)
  }

  toupper(trimws(country))
}

#' List Countries and Territorial Registers in EU-LOB
#'
#' @description
#' Lists the territorial codes represented in the bundled EU-LOB
#' catalogue and reports the number of distinct names and registered
#' entities associated with each code.
#'
#' @details
#' The catalogue contains national lobbying registers and the
#' supranational EU Transparency Register. The code `"UE"` identifies
#' the latter and is not an ISO country code.
#'
#' Counts are computed from the bundled catalogue. They describe
#' indexed records, not deduplicated legal entities across Europe.
#'
#' @param data_dir Must be `NULL`. External catalogue directories
#'   are not supported in this version.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{country}{Territorial or register code.}
#'     \item{n_names}{Number of distinct `name_id` values.}
#'     \item{n_entities}{Number of distinct register and
#'       `lobby_id` combinations within the code.}
#'   }
#'
#' @examples
#' eulob_countries()
#'
#' @seealso [eulob_registers()], [eulob_lobbies()]
#' @export
eulob_countries <- function(data_dir = NULL) {
  x <- .eulob_catalog("name_ids", data_dir)
  dt <- data.table::as.data.table(x)

  grouped <- dt[
    ,
    list(
      n_names = data.table::uniqueN(name_id),
      n_entities = data.table::uniqueN(.SD)
    ),
    by = "country",
    .SDcols = c("register", "lobby_id")
  ]

  grouped <- grouped[order(grouped[["country"]]), ]

  out <- data.frame(
    country = grouped[["country"]],
    n_names = grouped[["n_names"]],
    n_entities = grouped[["n_entities"]],
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL
  out
}

#' List Lobbying Registers in EU-LOB
#'
#' @description
#' Lists the lobbying registers included in the bundled catalogue,
#' optionally restricting the results to one territorial code.
#'
#' @details
#' Results are ordered by territorial code and register name.
#' The special code `"UE"` refers to the EU Transparency Register.
#' An unknown territorial code returns an empty data frame.
#'
#' @param country Optional single character string identifying a
#'   territorial code, such as `"ES"`, `"DE"` or `"UE"`.
#'   Leading and trailing whitespace is removed and the code
#'   is converted to uppercase.
#' @param data_dir Must be `NULL`. External catalogue directories
#'   are not supported in this version.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{country}{Territorial or register code.}
#'     \item{register}{Name of the lobbying register.}
#'     \item{n_names}{Number of distinct `name_id` values.}
#'     \item{n_entities}{Number of distinct `lobby_id` values
#'       within the register.}
#'   }
#'   If no register matches, the data frame has zero rows.
#'
#' @examples
#' eulob_registers()
#' eulob_registers(country = "ES")
#' eulob_registers(country = "UE")
#'
#' @seealso [eulob_countries()], [eulob_lobbies()]
#' @export
eulob_registers <- function(country = NULL,
                            data_dir = NULL) {
  x <- .eulob_catalog("name_ids", data_dir)
  country <- .eulob_country(country)

  dt <- data.table::as.data.table(x)

  if (!is.null(country)) {
    idx <- which(dt[["country"]] == country)
    dt <- dt[idx, ]
  }

  grouped <- dt[
    ,
    list(
      n_names = data.table::uniqueN(name_id),
      n_entities = data.table::uniqueN(lobby_id)
    ),
    by = c("country", "register")
  ]

  grouped <- grouped[
    order(grouped[["country"]], grouped[["register"]]),
    ,
  ]

  out <- data.frame(
    country = grouped[["country"]],
    register = grouped[["register"]],
    n_names = grouped[["n_names"]],
    n_entities = grouped[["n_entities"]],
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL
  out
}

#' Retrieve Lobbying Organization Names and Register Identifiers
#'
#' @description
#' Retrieves name-to-register-identifier relationships from the
#' bundled EU-LOB catalogue, with optional territorial and register
#' filters.
#'
#' @details
#' Each row represents a recorded relationship between a name and
#' an identifier in a lobbying register. Rows should not be
#' interpreted as globally deduplicated legal entities.
#'
#' `name_id` identifies a normalized name in the catalogue.
#' `lobby_id` is the identifier supplied for a register entry;
#' its format varies across registers and may be a URL.
#'
#' Results are ordered by normalized name, territorial code and
#' register identifier. Register filtering uses exact matching.
#'
#' @param country Optional territorial code such as `"ES"` or `"UE"`.
#'   The code is converted to uppercase.
#' @param register Optional single register name. Matching is exact
#'   and case-sensitive.
#' @param data_dir Must be `NULL`. External catalogue directories
#'   are not supported in this version.
#'
#' @return A data frame with six columns:
#'   \describe{
#'     \item{name_id}{Identifier associated with a normalized name.}
#'     \item{name}{Recorded organization name.}
#'     \item{name_normalized}{Normalized form used for searching
#'       and ordering.}
#'     \item{country}{Territorial or register code.}
#'     \item{register}{Name of the source register.}
#'     \item{lobby_id}{Register-specific entry identifier.}
#'   }
#'   Filters without matches return a zero-row data frame.
#'
#' @examples
#' head(eulob_lobbies(country = "ES"))
#' head(eulob_lobbies(
#'   country = "DE",
#'   register = "German Lobby Register"
#' ))
#'
#' @seealso [eulob_search()], [eulob_registers()]
#' @export
eulob_lobbies <- function(country = NULL, register = NULL,
                          data_dir = NULL) {
  x <- .eulob_catalog("name_ids", data_dir)
  country <- .eulob_country(country)

  if (!is.null(register)) {
    if (!is.character(register) ||
        length(register) != 1L ||
        is.na(register) ||
        !nzchar(trimws(register))) {
      stop("register must be a non-empty character string.",
           call. = FALSE)
    }
  }

  cache_key <- "lobbies_dt_order"

  if (!exists(cache_key, envir = .eulob_catalog_cache,
              inherits = FALSE)) {
    cached <- list(
      data = data.table::as.data.table(x),
      order = order(
        x$name_normalized,
        x$country,
        x$lobby_id
      )
    )

    assign(cache_key, cached, envir = .eulob_catalog_cache)
  }

  cached <- get(
    cache_key,
    envir = .eulob_catalog_cache,
    inherits = FALSE
  )

  dt <- cached$data
  positions <- cached$order

  if (!is.null(country)) {
    positions <- positions[
      dt[["country"]][positions] == country
    ]
  }

  if (!is.null(register)) {
    positions <- positions[
      dt[["register"]][positions] == register
    ]
  }

  cols <- c(
    "name_id", "name", "name_normalized",
    "country", "register", "lobby_id"
  )

  result <- as.data.frame(
    dt[positions, cols, with = FALSE]
  )

  attr(result, "datalabel") <- NULL
  attr(result, "var.labels") <- NULL
  rownames(result) <- NULL

  result
}

#' Search Lobbying Organization Names
#'
#' @description
#' Searches organization names in the bundled EU-LOB catalogue
#' using configurable string-matching methods.
#'
#' @details
#' The default method is cosine similarity using character bigrams
#' (`method = "cosine", q = 2L`).
#'
#' Approximate matching is implemented using the `stringdist`
#' package. The available methods are:
#'
#' - `"cosine"`: cosine similarity of character q-grams.
#' - `"jaccard"`: Jaccard similarity of character q-grams.
#' - `"jw"`: Jaro-Winkler similarity.
#' - `"dl"`: Damerau-Levenshtein similarity.
#' - `"fixed"`: case-insensitive literal substring matching.
#'
#' Approximate methods return a similarity score between 0 and 1,
#' where higher values indicate greater string similarity.
#'
#' When `min_similarity = NULL`, no minimum similarity threshold
#' is applied. Consequently, approximate methods may return
#' low-scoring or unrelated results when no close match exists.
#' A positive threshold can be specified to restrict the results.
#'
#' The `"fixed"` method preserves the original substring-search
#' behavior. It does not calculate similarity scores and ignores
#' the approximate-matching parameters.
#'
#' Results represent name-to-register-identifier relationships,
#' not necessarily unique legal entities. The same organization
#' name may appear in several registers.
#'
#' Approximate results are ordered by decreasing similarity,
#' followed by normalized name, territorial code and register
#' identifier. Fixed results are ordered by normalized name,
#' territorial code and register identifier.
#'
#' Similarity is lexical rather than semantic. High similarity
#' does not establish that two names refer to the same legal
#' entity.
#'
#' @param query Non-empty character string to search for.
#' @param country Optional territorial code, such as `"ES"` or
#'   `"UE"`. Converted to uppercase before filtering.
#' @param limit Positive whole number specifying the maximum
#'   number of returned rows. Default is `100L`.
#' @param data_dir Must be `NULL`. External catalogue directories
#'   are not supported in this version.
#' @param method Matching algorithm. One of `"cosine"` (default),
#'   `"jaccard"`, `"jw"`, `"dl"` or `"fixed"`.
#' @param min_similarity Optional minimum similarity between
#'   0 and 1. Default is `NULL`, meaning no threshold.
#'   Used only for approximate matching.
#' @param q Positive integer specifying the character q-gram
#'   length for cosine and Jaccard similarity. Default is `2L`.
#' @param p Jaro-Winkler prefix scaling factor between 0 and
#'   0.25. Default is `0.1`. Used only with `method = "jw"`.
#'
#' @return A data frame containing:
#'   \describe{
#'     \item{name_id}{Identifier associated with a normalized name.}
#'     \item{name}{Recorded organization name.}
#'     \item{country}{Territorial or register code.}
#'     \item{register}{Name of the source register.}
#'     \item{lobby_id}{Register-specific entry identifier.}
#'     \item{similarity}{Numeric similarity score between 0 and 1,
#'       included only for approximate matching methods.}
#'   }
#'   Results are limited to the requested number of rows.
#'   If no rows satisfy the search criteria, an empty data frame
#'   with the corresponding columns is returned.
#'
#' @examples
#' eulob_search("Siemens Mobilty", limit = 5)
#' eulob_search("association", method = "fixed", limit = 5)
#' eulob_search("university", country = "UE", limit = 5)
#' eulob_search("Siemens", method = "jw", limit = 5)
#' eulob_search("zzzxxyy_no_existe_123", method = "fixed")
#'
#' @seealso [eulob_lobbies()], [eulob_countries()]
#' @export
eulob_search <- function(query, country = NULL,
                         limit = 100L, data_dir = NULL,
                         method = "cosine",
                         min_similarity = NULL,
                         q = 2L, p = 0.1) {

  if (!is.character(query) ||
      length(query) != 1L ||
      is.na(query) ||
      !nzchar(trimws(query))) {
    stop("query must be a non-empty character string.",
         call. = FALSE)
  }

  if (!is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      !is.finite(limit) ||
      limit < 1 ||
      limit != floor(limit) ||
      limit > .Machine$integer.max) {
    stop("limit must be a positive integer.", call. = FALSE)
  }

  methods <- c("fixed", "jw", "dl", "cosine", "jaccard")

  if (!is.character(method) ||
      length(method) != 1L ||
      is.na(method) ||
      !method %in% methods) {
    stop(
      "method must be one of: ",
      paste(methods, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(min_similarity)) {
    if (!is.numeric(min_similarity) ||
        length(min_similarity) != 1L ||
        is.na(min_similarity) ||
        !is.finite(min_similarity) ||
        min_similarity < 0 ||
        min_similarity > 1) {
      stop("min_similarity must be between 0 and 1.",
           call. = FALSE)
    }
  }

  if (!is.numeric(q) ||
      length(q) != 1L ||
      is.na(q) ||
      !is.finite(q) ||
      q < 1 ||
      q != floor(q) ||
      q > .Machine$integer.max) {
    stop("q must be a positive integer.", call. = FALSE)
  }

  if (!is.numeric(p) ||
      length(p) != 1L ||
      is.na(p) ||
      !is.finite(p) ||
      p < 0 ||
      p > 0.25) {
    stop("p must be between 0 and 0.25.",
         call. = FALSE)
  }

  x <- data.table::as.data.table(
    .eulob_catalog("name_ids", data_dir)
  )
  country <- .eulob_country(country)

  if (!is.null(country)) {
    # Calcular fuera de DT[] para evitar colisiones de nombres.
    idx <- which(x[["country"]] == country)
    x <- x[idx, ]
  }

  # Exclude missing-name markers from searchable results.
  # The source records remain available through eulob_lobbies().
  idx <- which(
    !.eulob_is_placeholder_name(x[["name_normalized"]])
  )
  x <- x[idx, ]

  # Mantener intacta la búsqueda original.
  if (method == "fixed") {

    matches <- grepl(
      .eulob_normalize_query(query),
      x$name_normalized,
      fixed = TRUE
    )

    x <- x[which(matches), ]

    idx <- base::order(
      x[["name_normalized"]],
      x[["country"]],
      x[["lobby_id"]]
    )

    cols <- c(
      "name_id", "name", "country", "register", "lobby_id"
    )

    x <- as.data.frame(x[idx, cols, with = FALSE])
    attr(x, "datalabel") <- NULL
    attr(x, "var.labels") <- NULL

    x <- head(x, as.integer(limit))
    rownames(x) <- NULL
    return(x)
  }

  # Dependencia opcional: solo necesaria para búsqueda fuzzy.
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    stop(
      "Package 'stringdist' is required for approximate search.",
      call. = FALSE
    )
  }

  # Evitar calcular varias veces la misma denominación.
  candidates <- unique(x$name_normalized)

  if (anyNA(candidates)) {
    stop("Catalogue contains missing normalized names.",
         call. = FALSE)
  }

  params <- list(
    a = .eulob_normalize_query(query),
    b = candidates,
    method = method
  )

  if (method %in% c("cosine", "jaccard")) {
    params$q <- as.integer(q)
  }

  if (method == "jw") {
    params$p <- p
  }

  scores <- do.call(stringdist::stringsim, params)

  # Reasignar puntuaciones a todas las relaciones del catálogo.
  x$similarity <- scores[
    match(x$name_normalized, candidates)
  ]

  if (!is.null(min_similarity)) {
    idx <- which(
      !is.na(x[["similarity"]]) &
        x[["similarity"]] >= min_similarity
    )
    x <- x[idx, ]
  }

  idx <- base::order(
    -x[["similarity"]],
    x[["name_normalized"]],
    x[["country"]],
    x[["lobby_id"]]
  )

  cols <- c(
    "name_id", "name", "country",
    "register", "lobby_id", "similarity"
  )

  x <- as.data.frame(x[idx, cols, with = FALSE])
  attr(x, "datalabel") <- NULL
  attr(x, "var.labels") <- NULL

  x <- head(x, as.integer(limit))
  rownames(x) <- NULL
  x
}
