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
isScalar <- function(x) {
    testScalar(x)
}



#' @rdname isScalar
#' @importFrom rlang is_scalar_list
#' @export
isScalarList <- is_scalar_list



#' @rdname isScalar
#' @importFrom rlang is_scalar_atomic
#' @export
isScalarAtomic <- is_scalar_atomic



#' @rdname isScalar
#' @importFrom rlang is_scalar_vector
#' @export
isScalarVector <- is_scalar_vector



#' @rdname isScalar
#' @export
isScalarNumeric <- function(x) {
    isScalar(x) && is(x, "numeric")
}



#' @rdname isScalar
#' @importFrom rlang is_scalar_integer
#' @export
isScalarInteger <- is_scalar_integer



#' @rdname isScalar
#' @importFrom rlang is_scalar_integerish
#' @export
isScalarIntegerish <- is_scalar_integerish



#' @rdname isScalar
#' @importFrom rlang is_scalar_double
#' @export
isScalarDouble <- is_scalar_double



#' @rdname isScalar
#' @importFrom rlang is_scalar_character
#' @export
isScalarCharacter <- is_scalar_character



#' @rdname isScalar
#' @importFrom rlang is_scalar_logical
#' @export
isScalarLogical <- is_scalar_logical



#' @describeIn isScalar Alias for `isScalarCharacter`.
#' @importFrom rlang is_string
#' @export
isString <- is_string



#' @describeIn isScalar Alias for `isScalarLogical`.
#' @export
isFlag <- isScalarLogical



#' @describeIn isScalar Alias for `isScalarNumeric`.
#' @export
isNumber <- isScalarNumeric



#' @describeIn isScalar Alias for `isScalarIntegerish`.
#' @export
isInt <- isScalarIntegerish
