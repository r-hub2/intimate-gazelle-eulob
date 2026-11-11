#' Retrieve a Lobbying Register Entry by Identifier
#'
#' @description
#' Retrieves entries from the bundled EU-LOB catalogue using
#' an exact register-specific identifier.
#'
#' @details
#' Identifiers are compared literally, without normalization
#' or approximate matching. Leading zeros are preserved.
#'
#' A register entry may have more than one recorded name.
#' Consequently, multiple rows may be returned.
#'
#' Identifiers are source-specific and are not necessarily
#' official identifiers or globally unique legal entity IDs.
#'
#' @param country Territorial code, such as "GR", "PL" or "UE".
#' @param lobby_id Non-empty character string identifying
#'   the entry within its source register.
#' @param register Optional exact register name.
#' @param data_dir Must be NULL. External catalogue directories
#'   are not supported in this version.
#'
#' @return A data frame with the same columns as
#'   `eulob_lobbies()`. Returns zero rows when no entry matches.
#'
#' @examples
#' eulob_get("GR", "44")
#' eulob_get("PL", "00266")
#'
#' @seealso `eulob_lobbies()`, `eulob_search()`
#' @export
eulob_get <- function(country, lobby_id, register = NULL,
                      data_dir = NULL) {

  if (missing(country) || is.null(country)) {
    stop("country must be specified.", call. = FALSE)
  }

  if (missing(lobby_id) ||
      !is.character(lobby_id) ||
      length(lobby_id) != 1L ||
      is.na(lobby_id) ||
      !nzchar(trimws(lobby_id))) {
    stop("lobby_id must be a non-empty character string.",
         call. = FALSE)
  }

  x <- eulob_lobbies(
    country = country,
    register = register,
    data_dir = data_dir
  )

  x <- x[x$lobby_id == lobby_id, , drop = FALSE]
  rownames(x) <- NULL

  x
}
