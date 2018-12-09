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



#' @rdname isIntegerish
#' @importFrom rlang is_integerish
#' @export
isIntegerish <- is_integerish



#' @describeIn isIntegerish Short alias for `isScalarIntegerish`.
#' @importFrom rlang is_scalar_integerish
#' @export
isInt <- is_scalar_integerish
