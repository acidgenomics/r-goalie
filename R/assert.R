#' Assert that certain conditions are true
#'
#' [assert()] is a drop-in replacement for [`stopifnot()`][base::stopifnot]
#' supporting more informative error messages.
#'
#' @export
#'
#' @param ... Any number of R expressions that return `logical(1)`, each of
#'   which should evaluate to `TRUE`. Rather than combining expressions with
#'   `&&`, separate them by commas so that better error messages can be
#'   generated.
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
                    e[["call"]] <- call
                    stop(e)
                }
            ),
            warning = function(w) {
                w[["call"]] <- call
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
            # Always return a `stopifnot()`-like error.
            msg <- c(
                "Assert check failed.",
                sprintf("%s is not TRUE.", .Dparse(call))
            )
            # Check for defined cause attribute.
            cause <- cause(res)
            if (!is.null(cause)) {
                # Capturing the S3 print method on goalie class here.
                msg <- c(msg, capture.output(print(res))[-1L])
            }
            msg <- paste0(msg, collapse = "\n")
            stop(simpleError(msg, call = sys.call(-1L)))
        }
    }

    invisible(TRUE)
}
