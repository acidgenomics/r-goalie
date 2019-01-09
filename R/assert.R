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
#' @param msg `NULL` or `character(1)`.
#'   Custom message to return on the event of any check failure.
#' @param traceback `logical(1)`.
#'   Include traceback in error message.
#'   See [`traceback()`][base::traceback] for details.
#'
#' @seealso
#' - `stopifnot()`.
#' - `assertthat::assert_that()`.
#' - `assertive.base::assert_engine()`.
#' - `checkmate::assert()`.
#'
#' @return `TRUE` on success, error on failure.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assert <- function(..., msg = NULL, traceback = TRUE) {
    # Note that we're using `i` along with `...elt()` here to eval the call.
    dots <- as.call(substitute(...()))
    for (i in seq_along(dots)) {
        call <- dots[[i]]
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
        # This behavior differs from `stopifnot()` and is more consistent.
        if (!(is.logical(res) && length(res) == 1L)) {
            stop(paste(
                "Assert check failure.\n",
                "Check did not return a boolean flag (TRUE/FALSE).",
                .Dparse(call),
                sep = "\n"
            ))
        }

        # Stop on the first assert check failure.
        if (!isTRUE(res)) {
            # Note that we're allowing the user to define the message.
            if (!isString(msg)) {
                # Always return a `stopifnot()`-like error.
                msg <- c(
                    "Assert failure.",
                    sprintf("%s is not TRUE.", .Dparse(call))
                )
                # Check for defined cause attribute.
                cause <- cause(res)
                if (!is.null(cause)) {
                    # Capturing the S3 print method on goalie class here.
                    msg <- c(msg, capture.output(print(res))[-1L])
                }
                msg <- paste0(msg, collapse = "\n")
            }
            # Include the traceback in error by default.
            if (isTRUE(traceback)) {
                stack <- sys.calls()
                # FIXME stack <- as.character(unlist(stack))
                stack <- capture.output(print(stack))
                stack <- paste0(stack, collapse = "\n")
                msg <- paste(msg, "Traceback:", stack, sep = "\n")
            }
            stop(simpleError(msg, call = sys.call(-1L)))
        }
    }

    invisible(TRUE)
}
