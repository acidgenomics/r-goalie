#' Assert that certain conditions are true
#'
#' [assert()] is a drop-in replacement for [stopifnot()] supporting more
#' informative
#' error messages.
#'
#' @note Don't use [assertive::assert_that()] as a general [stopifnot()]
#'   replacement. It doesn't currently catch everything, and isn't hardened
#'   against S4 methods.
#'
#' @inheritParams base::stopifnot
#' @export
#'
#' @param ... Any number of R expressions that return `logical(1)`, each of
#'   which should evaluate to `TRUE`.
#'
#' @seealso
#' - `stopifnot()`.
#' - `assertthat::assert_that()`.
#' - `assertive.base::assert_engine()`.
#' - `checkmate::assert()`.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assert <- function(...) {
    mc <- match.call()[-1L]

    # Note that we're using `i` along with `...elt()` here to eval the call.
    for (i in seq_along(mc)) {
        call <- mc[[i]]
        res <- withCallingHandlers(
            expr = tryCatch(
                expr = ...elt(i),
                error = function(e) {
                    e$call <- call
                    stop(e)
                }
            ),
            warning = function(w) {
                w$call <- call
                w
            }
        )

        # Ensure that all check functions return boolean.
        # This behavior differs from stopifnot and is more consistent.
        if (!(is.logical(res) && length(res) == 1L)) {
            stop("All checks must return boolean flags.")
        }

        # Stop on the first assert check failure.
        if (!isTRUE(res)) {
            # Check for defined cause attribute.
            # Otherwise, generate a `stopifnot()`-like one automatically.
            cause <- cause(res)
            if (identical(cause, noquote(""))) {
                msg <- sprintf("%s is not TRUE", Dparse(call))
            } else {
                msg <- cause
            }
            stop(simpleError(msg, call = sys.call(-1L)))
        }
    }

    invisible(TRUE)
}
