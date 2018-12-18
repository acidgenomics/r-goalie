#' Do the inputs have the same length?
#'
#' @note Non-zero lengths for `x` and `y` are required, otherwise the check
#'   function will intentionally error.
#'
#' @export
#' @inherit params
#'
#' @examples
#' x <- list(a = 1, b = 2)
#' y <- list(c = 3, d = 4)
#' areSameLength(x = x, y = y)
areSameLength <- function(x, y) {
    assert(hasLength(x), hasLength(y))
    length(x) == length(y)
}
