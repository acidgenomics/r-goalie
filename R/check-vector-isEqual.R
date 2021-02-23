#' How does the input relate to a value?
#'
#' @name check-vector-isEqual
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - Primitive operators: `==`, `>`, `>=`, `<`, `<=`.
#' - `assertive.numbers::is_equal_to()`.
#' - `assertive.numbers::is_not_equal_to()`.
#' - `assertive.numbers::is_greater_than()`.
#' - `assertive.numbers::is_greater_than_or_equal_to()`.
#' - `assertive.numbers::is_less_than()`.
#' - `assertive.numbers::is_less_than_or_equal_to()`.
#'
#' @examples
#' ## TRUE ====
#' isEqualTo(x = 1L, y = 1)
#' isNotEqualTo(x = 2, y = 1)
#' isGreaterThan(x = 1, y = 0)
#' isGreaterThanOrEqualTo(x = seq_len(2), y = 1)
#' isLessThan(x = -1, y = 0)
#' isLessThanOrEqualTo(x = seq_len(2), y = 3)
#'
#' ## FALSE ====
#' isEqualTo(x = seq_len(2), y = 1)
NULL



## Vector ======================================================================
#' @describeIn check-vector-isEqual Vectorized.
#' @export
isEqualTo <- function(x, y) {
    diff <- abs(x - y)
    ok <- diff <= .tolerance
    names(ok) <- .toNames(x)
    setCause(ok, sprintf("not equal to %g; abs diff = %g", y, diff))
}



#' @describeIn check-vector-isEqual Vectorized.
#' @export
isNotEqualTo <- function(x, y) {
    ok <- abs(x - y) > .tolerance
    names(ok) <- .toNames(x)
    setCause(ok, sprintf("equal to %g", y))
}



#' @describeIn check-vector-isEqual Vectorized.
#' @export
isGreaterThan <- function(x, y) {
    ok <- x > y
    names(ok) <- .toNames(x)
    setCause(ok, false = paste("less than or equal to", y))
}



#' @describeIn check-vector-isEqual Vectorized.
#' @export
isGreaterThanOrEqualTo <- function(x, y) {
    ok <- x >= y
    names(ok) <- .toNames(x)
    setCause(ok, false = paste("less than", y))
}



#' @describeIn check-vector-isEqual Vectorized.
#' @export
isLessThan <- function(x, y) {
    ok <- x < y
    names(ok) <- .toNames(x)
    setCause(ok, false = paste("greater than or equal to", y))
}



#' @describeIn check-vector-isEqual Vectorized.
#' @export
isLessThanOrEqualTo <- function(x, y) {
    ok <- x <= y
    names(ok) <- .toNames(x)
    setCause(ok, false = paste("greater than", y))
}



## Scalar ======================================================================
#' @describeIn check-vector-isEqual Scalar.
#' @export
allAreEqualTo <- function(x, y) {
    ok <- isEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isEqual Scalar.
#' @export
allAreNotEqualTo <- function(x, y) {
    ok <- isNotEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isEqual Scalar.
#' @export
allAreGreaterThan <- function(x, y) {
    ok <- isGreaterThan(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isEqual Scalar.
#' @export
allAreGreaterThanOrEqualTo <- function(x, y) {
    ok <- isGreaterThanOrEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isEqual Scalar.
#' @export
allAreLessThan <- function(x, y) {
    ok <- isLessThan(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn check-vector-isEqual Scalar.
#' @export
allAreLessThanOrEqualTo <- function(x, y) {
    ok <- isLessThanOrEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
