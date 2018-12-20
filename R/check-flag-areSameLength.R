#' Do the inputs have the same length?
#'
#' @note Non-zero lengths for `x` and `y` are required, otherwise the check
#'   function will intentionally error.
#'
#' @export
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' x <- list(a = 1, b = 2)
#' y <- list(c = 3, d = 4)
#' areSameLength(x = x, y = y)
#'
#' ## Fail ====
#' x <- list(a = 1)
#' y <- list(b = 2, c = 3)
#' areSameLength(x = x, y = y)
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
