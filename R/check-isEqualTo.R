#' How Does the Input Relate to a Value?
#'
#' @note These functions return `logical`, not necessarily `logical(1)`.
#'
#' @name isEqualTo
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



#' @rdname isEqualTo
#' @export
isEqualTo <- function(x, y) {
    isTRUE(all.equal(target = x, current = y))
}



#' @rdname isEqualTo
#' @export
isNotEqualTo <- function(x, y) {
    !isEqualTo(x, y)
}



#' @rdname isEqualTo
#' @export
isGreaterThan <- function(x, y) {
    x > y
}



#' @rdname isEqualTo
#' @export
isGreaterThanOrEqualTo <- function(x, y) {
    x >= y
}



#' @rdname isEqualTo
#' @export
isLessThan <- function(x, y) {
    x < y
}



#' @rdname isEqualTo
#' @export
isLessThanOrEqualTo <- function(x, y) {
    x <= y
}
