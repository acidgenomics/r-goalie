#' Is the Input Integerish?
#'
#' Check for valid input of either explicit (e.g. `1L`) and/or implict
#' (e.g. `1`) `integer`.
#'
#' @export
#' @inherit params
#'
#' @seealso
#' - `rlang::is_integerish()`.
#' - `checkmate::checkIntegerish()`.
#'
#' @examples
#' isIntegerish(seq_len(2L))
#' isIntegerish(c(1, 2))
isIntegerish <- function(x) {
    requireNamespace("rlang", quietly = TRUE)
    rlang::is_integerish(x)
}
