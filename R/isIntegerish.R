#' Is Integerish?
#'
#' Check for valid input of either explicit (e.g. `1L`) and/or implict
#' (e.g. `1`) integer.
#'
#' @name isIntegerish
#' @inherit params
#' @inheritParams rlang::is_integerish
#'
#' @seealso
#' - `rlang::is_integerish()`.
#' - `checkmate::checkIntegerish`.
#' - `assertive::is_a_number`.
#'
#' @examples
#' isIntegerish(seq_len(2L))
#' isIntegerish(c(1, 2))
NULL



#' @rdname isIntegerish
#' @importFrom rlang is_integerish
#' @export
isIntegerish <- is_integerish
