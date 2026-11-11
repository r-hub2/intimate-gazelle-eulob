#' EU-LOB: European Union Lobbying Organizations Database
#'
#' @description
#' Provides tools for accessing and exploring a bundled
#' catalogue of registered lobbying organizations in
#' the European Union.
#'
#' @details
#' The catalogue covers 19 territorial scopes and 21 registers,
#' including the supranational EU Transparency Register.
#' The Netherlands is not included in the searchable catalogue
#' because its available historical records do not support
#' reliable identification of lobbying entities.
#'
#' The catalogue distinguishes normalized organization
#' names from register-specific entity identifiers.
#' Records are not globally deduplicated legal entities.
#'
#' Data are stored locally and can be queried without
#' an internet connection.
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{eulob_countries()}: Territorial coverage.
#'   \item \code{eulob_registers()}: Available registers.
#'   \item \code{eulob_lobbies()}: Name and identifier records.
#'   \item \code{eulob_search()}: Search organization names.
#'   \item \code{eulob_cross_registers()}: Shared names across registers.
#'   \item \code{eulob_info()}: Project metadata.
#'   \item \code{eulob_version()}: Package version.
#' }
#'
#' @seealso
#' \code{\link{eulob_countries}},
#' \code{\link{eulob_registers}},
#' \code{\link{eulob_lobbies}},
#' \code{\link{eulob_search}},
#' \code{\link{eulob_cross_registers}}
#'
#' @keywords internal
"_PACKAGE"
