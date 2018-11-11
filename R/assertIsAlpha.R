#' Assert Is Alpha Level
#'
#' An alpha level must be between 0 and 1, but not equal either 0 or 1.
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertIsAlpha(0.05)
assertIsAlpha <- function(object) {
    assert_is_a_number(object)
    assert_all_are_in_open_range(object, lower = 0L, upper = 1L)
    if (object > 0.1) {
        warning("An alpha level above 0.1 (10%) is not recommended.")
    }
}
