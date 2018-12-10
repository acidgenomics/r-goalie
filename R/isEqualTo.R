#' How Does the Input Relate to a Value?
#'
#' @note These functions return `logical`, not necessarily `logical(1)`.
#'
#' @name isEqualTo
#' @inherit params
#' @inheritParams assertive.numbers::is_equal_to
#'
#' @return `logical`.
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
#' @importFrom assertive.numbers is_equal_to
#' @export
isEqualTo <- is_equal_to



#' @rdname isEqualTo
#' @importFrom assertive.numbers is_not_equal_to
#' @export
isNotEqualTo <- is_not_equal_to



#' @rdname isEqualTo
#' @importFrom assertive.numbers is_greater_than
#' @export
isGreaterThan <- is_greater_than



#' @rdname isEqualTo
#' @importFrom assertive.numbers is_greater_than_or_equal_to
#' @export
isGreaterThanOrEqualTo <- is_greater_than_or_equal_to



#' @rdname isEqualTo
#' @importFrom assertive.numbers is_less_than
#' @export
isLessThan <- is_less_than



#' @rdname isEqualTo
#' @importFrom assertive.numbers is_less_than_or_equal_to
#' @export
isLessThanOrEqualTo <- is_less_than_or_equal_to
