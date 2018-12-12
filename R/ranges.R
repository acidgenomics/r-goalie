# TODO Consider droping the `is` prefix from these?

#' Is the Input in Range?
#'
#' @name ranges
#' @inherit params
#' @inheritParams assertive.numbers::is_in_range
#'
#' @examples
#' ## Pass ====
#' isInRange(x = 1, lower = 0, upper = 1)
#'
#' isInClosedRange(x = 0, lower = 0, upper = 1)
#' isInClosedRange(x = 1, lower = 0, upper = 1)
#'
#' isInOpenRange(x = 0.5, lower = 0, upper = 1)
#'
#' isInLeftOpenRange(x = 1, lower = 0, upper = 1)
#' isInRightOpenRange(x = 0, lower = 0, upper = 1)
#'
#' ## Fail ====
#' isInRange(x = 2, lower = 0, upper = 1)
#'
#' isInOpenRange(x = 0, lower = 0, upper = 1)
#' isInOpenRange(x = 1, lower = 0, upper = 1)
#'
#' isInLeftOpenRange(x = 0, lower = 0, upper = 1)
#' isInRightOpenRange(x = 1, lower = 0, upper = 1)
NULL


#' @rdname ranges
#' @importFrom assertive.numbers is_in_range
#' @export
isInRange <- is_in_range



#' @rdname ranges
#' @importFrom assertive.numbers is_in_closed_range
#' @export
isInClosedRange <- is_in_closed_range



#' @rdname ranges
#' @importFrom assertive.numbers is_in_open_range
#' @export
isInOpenRange <- is_in_open_range



#' @rdname ranges
#' @importFrom assertive.numbers is_in_left_open_range
#' @export
isInLeftOpenRange <- is_in_left_open_range


#' @rdname ranges
#' @importFrom assertive.numbers is_in_right_open_range
#' @export
isInRightOpenRange <- is_in_right_open_range



#' @rdname ranges
#' @importFrom assertive.numbers is_negative
#' @export
isNegative <- is_negative



#' @rdname ranges
#' @importFrom assertive.numbers is_non_negative
#' @export
isNonNegative <- is_non_negative



#' @rdname ranges
#' @importFrom assertive.numbers is_positive
#' @export
isPositive <- is_positive



#' @rdname ranges
#' @importFrom assertive.numbers is_non_positive
#' @export
isNonPositive <- is_non_positive



#' @rdname ranges
#' @importFrom assertive.numbers is_percentage
#' @export
isPercentage <- is_percentage



#' @rdname ranges
#' @importFrom assertive.numbers is_proportion
#' @export
isProportion <- is_proportion
