# TODO Consider allowing `isInt` to only return true for non-negative
# or positive values.



#' Is the Input Scalar?
#'
#' Scalar represents a length of 1.
#'
#' @name isScalar
#' @inherit params
#' @inheritParams assertive.properties::is_scalar
#' @inheritParams rlang::is_scalar_character
#' @inheritParams rlang::is_scalar_integerish
#'
#' @seealso
#' `help(topic = "scalar-type-predicates", package = "rlang")`.
#' - `assertive.properties::is_scalar()`.
#' - `rlang::is_scalar_list()`.
#' - `rlang::is_scalar_atomic()`.
#' - `rlang::is_scalar_vector()`.
#' - `rlang::is_scalar_integer()`.
#' - `rlang::is_scalar_integerish()`.
#' - `rlang::is_scalar_double()`.
#' - `rlang::is_scalar_character()`.
#' - `rlang::is_scalar_logical()`.
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
#' @export
isScalar <- function(x) {
    length(x) == 1L
}



#' @rdname isScalar
#' @export
isNonScalar <- function(x) {
    !isScalar(x)
}



#' @rdname isScalar
#' @export
isScalarList <- function(x) {
    isScalar(x) && is.list(x)
}



#' @rdname isScalar
#' @export
isScalarAtomic <- function(x) {
    isScalar(x) && is.atomic(x)
}



#' @rdname isScalar
#' @export
isScalarVector <- function(x) {
    isScalar(x) && is.vector(x)
}



#' @rdname isScalar
#' @export
isScalarNumeric <- function(x) {
    isScalar(x) && is.numeric(x)
}



#' @rdname isScalar
#' @export
isScalarInteger <- function(x) {
    isScalar(x) && is.integer(x)
}



#' @rdname isScalar
#' @export
isScalarIntegerish <- function(x) {
    isScalar(x) && isIntegerish(x)
}



#' @rdname isScalar
#' @export
isScalarDouble <- function(x) {
    isScalar(x) && is.double(x)
}



#' @rdname isScalar
#' @export
isScalarCharacter <- function(x) {
    isScalar(x) && is.character(x)
}



#' @rdname isScalar
#' @export
isScalarLogical <- function(x) {
    isScalar(x) && is.logical(x)
}



#' @describeIn isScalar Alias for `isScalarNumeric`.
#' @export
isNumber <- isScalarNumeric



#' @describeIn isScalar Alias for `isScalarIntegerish`.
#' @export
isInt <- isScalarIntegerish
