#' Registers represented in the EU-LOB catalogue
#'
#' Return the registers and their counts from the package's
#' existing catalogue indexes. No external metadata file is needed.
#'
#' @param country Optional character vector of country codes.
#' @param register Optional character vector of register names.
#' @param fields Optional character vector of columns to return.
#'
#' @return A data.frame containing country, register, n_names
#'   and n_entities, or a selection of these columns.
#'
#' @details
#' Counts are derived from the existing catalogue. This function
#' does not provide institutional URLs, licences or snapshot
#' statistics, which are not contained in these indexes.
#'
#' @examples
#' eulob_sources()
#' eulob_sources(country = "ES")
#' eulob_sources(country = "IT")
#' eulob_sources(fields = c("country", "register"))
#'
#' @seealso [eulob_registers()]
#' @export
eulob_sources <- function(country = NULL, register = NULL, fields = NULL) {
  validate_filter <- function(x, argument) {
    if (is.null(x)) return(invisible(NULL))

    if (!is.character(x) || anyNA(x) ||
        any(!nzchar(trimws(x)))) {
      stop(
        argument, " must contain non-empty character values.",
        call. = FALSE
      )
    }

    invisible(NULL)
  }

  validate_filter(country, "country")
  validate_filter(register, "register")
  validate_filter(fields, "fields")

  sources <- eulob_registers()

  if (!is.null(country)) {
    country <- toupper(trimws(country))
    sources <- sources[
      sources$country %in% country, , drop = FALSE
    ]
  }

  if (!is.null(register)) {
    sources <- sources[
      sources$register %in% register, , drop = FALSE
    ]
  }

  if (!is.null(fields)) {
    unknown <- setdiff(fields, names(sources))

    if (length(unknown)) {
      stop(
        "Unknown fields: ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }

    sources <- sources[, fields, drop = FALSE]
  }

  rownames(sources) <- NULL
  sources
}
