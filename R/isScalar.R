#' Is Scalar?
#'
#' @name isScalar
#' @inherit params
#' @inheritParams checkmate::testScalar
#' @inheritParams rlang::is_scalar_integerish
#'
#' @seealso
#' `help(topic = "scalar-type-predicates", package = "rlang")`.
#'
#' @examples
#' ## Pass ====
#' isScalar("a")
#' isScalarInteger(1L)
#' isScalarIntegerish(1)
#'
#' ## Fail ====
#' isScalar(NULL)
#' isScalar(c("a", "b"))
NULL



#' @rdname isScalar
#' @importFrom checkmate testScalar
#' @export
isScalar <- testScalar



#' @rdname isScalar
#' @importFrom rlang is_scalar_integer
#' @export
isScalarInteger <- is_scalar_integer



#' @rdname isScalar
#' @importFrom rlang is_scalar_integerish
#' @export
isScalarIntegerish <- is_scalar_integerish
