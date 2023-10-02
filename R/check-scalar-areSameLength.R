#' Do the inputs have the same length?
#'
#' @note Non-zero lengths for `x` and `y` are required, otherwise the check
#' function will intentionally error.
#'
#' @name check-scalar-areSameLength
#' @note Updated 2023-09-29.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
areSameLength <-
    function(x, y) {
        ok <- hasLength(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- hasLength(y)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- identical(length(x), length(y))
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} doesn't have the same length as {.var %s}.",
                .toName(x), .toName(y)
            ))
        }
        TRUE
    }
