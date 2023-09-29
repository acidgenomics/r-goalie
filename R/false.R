#' Set a false goalie check with cause attribute
#'
#' @export
#' @note Updated 2023-09-29.
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
    goalie(object = FALSE, cause = sprintf(...))
}
