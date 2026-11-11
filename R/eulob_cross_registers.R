#' Find Organization Names Shared Across Lobbying Registers
#'
#' Identify names occurring in multiple territorial lobbying
#' registers in the bundled EU-LOB catalogue.
#'
#' @details
#' Matching uses normalized organization names and existing
#' name identifiers. A shared name is not proof that registrations
#' refer to the same legal entity.
#'
#' The EU Transparency Register ("UE") counts as a separate scope.
#' The limit is applied to names, not registration rows.
#'
#' @param name Optional organization name for exact normalized
#'   matching. Matching is case-insensitive, but not fuzzy.
#' @param min_countries Positive integer specifying the minimum
#'   number of territorial scopes. Default is 2.
#' @param limit Optional positive integer limiting distinct names.
#'
#' @return A data frame containing name_id, name, country,
#'   register, lobby_id, n_countries and n_entities.
#'
#' @examples
#' eulob_cross_registers(limit = 5)
#' eulob_cross_registers("Grünenthal GmbH")
#' eulob_cross_registers(min_countries = 3, limit = 10)
#'
#' @seealso [eulob_lobbies()], [eulob_search()]
#' @export
eulob_cross_registers <- function(
    name = NULL,
    min_countries = 2L,
    limit = NULL) {

  valid_integer <- function(x) {
    is.numeric(x) &&
      length(x) == 1L &&
      !is.na(x) &&
      is.finite(x) &&
      x >= 1 &&
      x == floor(x) &&
      x <= .Machine$integer.max
  }

  if (!valid_integer(min_countries)) {
    stop("min_countries must be a positive integer.",
         call. = FALSE)
  }

  if (!is.null(limit) && !valid_integer(limit)) {
    stop("limit must be NULL or a positive integer.",
         call. = FALSE)
  }

  if (!is.null(name)) {
    if (!is.character(name) ||
        length(name) != 1L ||
        is.na(name) ||
        !nzchar(trimws(name))) {
      stop("name must be a non-empty character string.",
           call. = FALSE)
    }
  }

  catalog <- data.table::as.data.table(
    .eulob_catalog("names", NULL)
  )

  idx <- which(
    !.eulob_is_placeholder_name(catalog[["name_normalized"]]) &
      !is.na(catalog[["n_countries"]]) &
      catalog[["n_countries"]] >= min_countries
  )

  selected <- catalog[idx, ]

  if (!is.null(name)) {
    normalized <- tolower(trimws(name))
    idx <- which(selected[["name_normalized"]] == normalized)
    selected <- selected[idx, ]
  }

  # Mantener exactamente la ordenación original de R base.
  idx <- base::order(
    -selected[["n_countries"]],
    -selected[["n_entities"]],
    selected[["name_normalized"]],
    selected[["name_id"]]
  )

  selected <- selected[idx, ]

  if (!is.null(limit)) {
    selected <- head(selected, as.integer(limit))
  }

  ids <- data.table::as.data.table(
    .eulob_catalog("name_ids", NULL)
  )

  idx <- which(ids[["name_id"]] %in% selected[["name_id"]])
  ids <- ids[idx, ]

  if (nrow(ids)) {
    # También conservar el orden original dentro de cada nombre.
    idx <- base::order(
      match(ids[["name_id"]], selected[["name_id"]]),
      ids[["country"]],
      ids[["lobby_id"]]
    )

    ids <- ids[idx, ]
  }

  metadata <- selected[
    match(ids[["name_id"]], selected[["name_id"]]),
    c("n_countries", "n_entities"),
    with = FALSE
  ]

  result <- data.frame(
    name_id = ids[["name_id"]],
    name = ids[["name"]],
    country = ids[["country"]],
    register = ids[["register"]],
    lobby_id = ids[["lobby_id"]],
    n_countries = metadata[["n_countries"]],
    n_entities = metadata[["n_entities"]],
    stringsAsFactors = FALSE
  )

  rownames(result) <- NULL
  result
}
