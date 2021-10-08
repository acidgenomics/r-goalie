#' Set a false goalie check with cause attribute
#'
#' @export
#' @note Updated 2021-02-23.
#'
#' @param ... Elements to pass to internal [`sprintf()`][base::sprintf] call.
#'
#' @return `goalie`.
#'
#' @examples
#' x <- false("{.var %s} is invalid.", "xxx")
#' print(x)
#' print(cause(x))
false <- function(...) {
    stopifnot(isTRUE(nargs() > 0L))
    goalie(object = FALSE, cause = sprintf(...))
}
