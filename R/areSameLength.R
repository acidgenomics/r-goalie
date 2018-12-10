#' Do the Inputs Have the Same Length?
#'
#' @inherit params
#' @export
#'
#' @examples
#' x <- list(a = 1, b = 2)
#' y <- list(c = 3, d = 4)
#' areSameLength(x = x, y = y)
areSameLength <- function(x, y) {
    assert(hasLength(x), hasLength(y))
    length(x) == length(y)
}
