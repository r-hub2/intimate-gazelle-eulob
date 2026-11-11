#' Information About the EU-LOB Database
#'
#' Returns basic information about the EU-LOB project
#' and the installed R package.
#'
#' @return A named list containing project metadata.
#' @export
#'
#' @examples
#' eulob_info()
eulob_info <- function() {
  list(
    project = "EU-LOB",
    full_name = paste(
      "An Open and Incremental Database of",
      "Registered Lobbying Organizations",
      "in the European Union"
    ),
    package = "eulob",
    version = as.character(utils::packageVersion("eulob")),
    repository = NA_character_,
    data_version = NA_character_
  )
}
