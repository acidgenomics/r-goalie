#' Do the inputs have the same length?
#'
#' @note Non-zero lengths for `x` and `y` are required, otherwise the check
#'   function will intentionally error.
#'
#' @name check-scalar-areSameLength
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
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
NULL



#' @rdname check-scalar-areSameLength
#' @export
areSameLength <- function(
    x, y,
    .xname = getNameInParent(x),
    .yname = getNameInParent(y)
) {
    ok <- hasLength(x = x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)  # nocov

    ok <- hasLength(x = y, .xname = .yname)
    if (!isTRUE(ok)) return(ok)  # nocov

    ok <- identical(length(x), length(y))
    if (!isTRUE(ok)) {
        return(false(
            "'%s' does not have the same length as '%s'.",
            .xname, .yname
        ))
    }

    TRUE
}
