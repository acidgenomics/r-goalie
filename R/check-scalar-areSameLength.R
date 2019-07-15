#' Do the inputs have the same length?
#'
#' @note Non-zero lengths for `x` and `y` are required, otherwise the check
#'   function will intentionally error.
#'
#' @export
#' @inherit params
#'
#' @examples
#' ## TRUE ====
#' x <- list(a = 1L, b = 2L)
#' y <- list(c = 3L, d = 4L)
#' areSameLength(x = x, y = y)
#'
#' ## FALSE ====
#' x <- list(a = 1L)
#' y <- list(b = 2L, c = 3L)
#' areSameLength(x = x, y = y)

# Updated 2019-07-15.
areSameLength <- function(
    x, y,
    .xname = getNameInParent(x),
    .yname = getNameInParent(y)
) {
    ok <- hasLength(x = x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- hasLength(x = y, .xname = .yname)
    if (!isTRUE(ok)) return(ok)

    ok <- identical(length(x), length(y))
    if (!isTRUE(ok)) {
        return(false(
            "%s does not have the same length as %s.",
            .xname, .yname
        ))
    }

    TRUE
}
