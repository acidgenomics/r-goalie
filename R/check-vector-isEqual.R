#' How does the input relate to a value?
#'
#' @note These functions return `logical`, not necessarily `logical(1)`.
#'
#' @name isEqual
#' @inherit params
#'
#' @return `logical`.
#'
#' @seealso
#' - Primitives: `==`, `>`, `>=`, `<`, `<=`.
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



# vector =======================================================================
#' @describeIn isEqual Vectorized.
#' @export
# Updated 2019-07-15.
isEqualTo <- function(x, y) {
    diff <- abs(x - y)
    ok <- diff <= .tolerance
    callAndName(
        function(x) {
            setCause(ok, sprintf("not equal to %g; abs diff = %g", y, diff))
        },
        rep_len(x, length(ok))
    )
}



#' @describeIn isEqual Vectorized.
#' @export
# Updated 2019-07-15.
isNotEqualTo <- function(x, y) {
    ok <- abs(x - y) > .tolerance
    callAndName(
        function(x) {
            setCause(ok, sprintf("equal to %g", y))
        },
        rep_len(x, length(ok))
    )
}



#' @describeIn isEqual Vectorized.
#' @export
# Updated 2019-07-15.
isGreaterThan <- function(x, y) {
    ok <- x > y
    callAndName(
        function(x) {
            setCause(ok, false = paste("less than or equal to", y))
        },
        rep_len(x, length(ok))
    )
}



#' @describeIn isEqual Vectorized.
#' @export
# Updated 2019-07-15.
isGreaterThanOrEqualTo <- function(x, y) {
    ok <- x >= y
    callAndName(
        function(x) {
            setCause(ok, false = paste("less than", y))
        },
        rep_len(x, length(ok))
    )
}



#' @describeIn isEqual Vectorized.
#' @export
# Updated 2019-07-15.
isLessThan <- function(x, y) {
    ok <- x < y
    callAndName(
        function(x) {
            setCause(ok, false = paste("greater than or equal to", y))
        },
        rep_len(x, length(ok))
    )
}



#' @describeIn isEqual Vectorized.
#' @export
# Updated 2019-07-15.
isLessThanOrEqualTo <- function(x, y) {
    ok <- x <= y
    callAndName(
        function(x) {
            setCause(ok, false = paste("greater than", y))
        },
        rep_len(x, length(ok))
    )
}



# scalar =======================================================================
#' @describeIn isEqual Scalar.
#' @export
# Updated 2019-07-15.
allAreEqualTo <- function(x, y) {
    ok <- isEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn isEqual Scalar.
#' @export
# Updated 2019-07-15.
allAreNotEqualTo <- function(x, y) {
    ok <- isNotEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn isEqual Scalar.
#' @export
# Updated 2019-07-15.
allAreGreaterThan <- function(x, y) {
    ok <- isGreaterThan(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn isEqual Scalar.
#' @export
# Updated 2019-07-15.
allAreGreaterThanOrEqualTo <- function(x, y) {
    ok <- isGreaterThanOrEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn isEqual Scalar.
#' @export
# Updated 2019-07-15.
allAreLessThan <- function(x, y) {
    ok <- isLessThan(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



#' @describeIn isEqual Scalar.
#' @export
# Updated 2019-07-15.
allAreLessThanOrEqualTo <- function(x, y) {
    ok <- isLessThanOrEqualTo(x, y)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}
