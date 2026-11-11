#' Get the Version of the EU-LOB R Package
#'
#' Returns the installed version of the eulob package.
#'
#' @return A package_version object.
#' @export
#'
#' @examples
#' eulob_version()
eulob_version <- function() {
  utils::packageVersion("eulob")
}
