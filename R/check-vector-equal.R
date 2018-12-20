#' How does the input relate to a value?
#'
#' @note These functions return `logical`, not necessarily `logical(1)`.
#'
#' @name equal
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
#' ## Pass ====
#' isEqualTo(x = 1L, y = 1)
#' isNotEqualTo(x = 2, y = 1)
#' isGreaterThan(x = 1, y = 0)
#' isGreaterThanOrEqualTo(x = seq_len(2), y = 1)
#' isLessThan(x = -1, y = 0)
#' isLessThanOrEqualTo(x = seq_len(2), y = 3)
#'
#' ## Fail ====
#' isEqualTo(x = seq_len(2), y = 1)
NULL



#' @rdname equal
#' @export
isEqualTo <- function(x, y) {
    diff <- abs(x - y)
    tol <- 100L * .Machine$double.eps
    ok <- diff <= tol
    callAndName(
        function(x) {
            setCause(ok, sprintf("not equal to %g; abs diff = %g", y, diff))
        },
        rep_len(x, length(ok))
    )
}



#' @rdname equal
#' @export
isNotEqualTo <- function(x, y) {
    tol <- 100L * .Machine$double.eps
    ok <- abs(x - y) > tol
    callAndName(
        function(x) {
            setCause(ok, sprintf("equal to %g", y))
        },
        rep_len(x, length(ok))
    )
}



#' @rdname equal
#' @export
isGreaterThan <- function(x, y) {
    ok <- x > y
    callAndName(
        function(x) {
            setCause(ok, false = paste("less than or equal to", y))
        },
        rep_len(x, length(ok))
    )
}



#' @rdname equal
#' @export
isGreaterThanOrEqualTo <- function(x, y) {
    ok <- x >= y
    callAndName(
        function(x) {
            setCause(ok, false = paste("less than", y))
        },
        rep_len(x, length(ok))
    )
}



#' @rdname equal
#' @export
isLessThan <- function(x, y) {
    ok <- x < y
    callAndName(
        function(x) {
            setCause(ok, false = paste("greater than or equal to", y))
        },
        rep_len(x, length(ok))
    )
}



#' @rdname equal
#' @export
isLessThanOrEqualTo <- function(x, y) {
    ok <- x <= y
    callAndName(
        function(x) {
            setCause(ok, false = paste("greater than", y))
        },
        rep_len(x, length(ok))
    )
}
