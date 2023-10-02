#' Define a goalie check classed return
#'
#' @export
#' @note Updated 2023-09-29.
#'
#' @param object `logical`.
#'
#' @param cause `character`.
#' Only applies when `object` vector contains any `FALSE` values.
#' The `cause` attribute must be named.
#' Cause values corresponding to `TRUE` must be set `NA`.
#'
#' @return
#' All `TRUE`: `logical`.
#' Any `FALSE`: `goalie`, which extends `logical`.
#'
#' @examples
#' ## Any FALSE.
#' x <- goalie(
#'     object = c(FALSE, TRUE),
#'     cause = c(
#'         "1" = "failed check",
#'         "2" = NA_character_
#'     )
#' )
#' print(x)
#' print(cause(x))
#'
#' ## All TRUE.
#' x <- goalie(rep(TRUE, 2L))
#' print(x)
goalie <- function(object, cause) {
    if (all(object)) {
        if (identical(length(object), 0L)) {
            stop("Invalid input.")
        }
        object <- unname(object)
        return(object)
    }
    new(Class = "goalie", ".Data" = object, "cause" = cause)
}
