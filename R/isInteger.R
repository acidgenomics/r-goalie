#' @name isInteger
#' @inherit params
#'
#' @seealso
#' - `rlang::is_integerish()`.
#' - `rlang::is_scalar_integer()`.
#' - `rlang::is_scalar_integerish()`.
#' - `checkmate::checkIntegerish`.
#' - `checkmate::checkInt`.
#' - `assertive::is_a_number`.
#'
#' @examples
#' isIntegerish(c(1, 2))
#'
#' ## Requires scalar integerish.
#' isInt(1)
#' isInt(1L)
NULL



#' @rdname isInteger
#' @importFrom rlang is_integerish
#' @export
isIntegerish <- is_integerish



#' @rdname isInteger
#' @importFrom rlang is_scalar_integerish
#' @export
isInt <- is_scalar_integerish
