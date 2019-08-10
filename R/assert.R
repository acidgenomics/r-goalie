#' Assert that certain conditions are true
#'
#' [assert()] is a drop-in replacement for [`stopifnot()`][base::stopifnot]
#' supporting more informative error messages.
#'
#' If any of the expressions defined in `...` are not [`all`](base::all) `TRUE`,
#' [`stop`][base::stop] is called, producing an error message indicating the
#' first expression which was not `TRUE`.
#'
#' @note Updated 2019-08-10.
#' @export
#'
#' @inheritParams acidroxygen::params
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
#' @return `TRUE` on success, error on failure.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assert <- function(
    ...,
    msg = NULL,
    traceback = getOption("acid.traceback", default = FALSE)
) {
    n <- ...length()
    if (n == 0L) {
        stop("No assert check defined.")
    }
    dots <- as.call(substitute(...()))

    for (i in seq_len(n)) {
        r <- ...elt(i)
        ## Ensure we're stripping names off of logical. Otherwise,
        ## `isTRUE()` check will fail on R 3.4.
        r <- unname(r)
        call <- .Dparse(dots[[i]])

        if (!(is.logical(r) && length(r) == 1L)) {
            stop(sprintf(
                paste0(
                    "Assert failure.\n",
                    "Check did not return a boolean flag (TRUE/FALSE).\n",
                    "[%s]: %s"
                ),
                i, call
            ))
        } else if (isTRUE(r)) {
            next
        }

        ## Note that we're allowing the user to define the message.
        if (!isString(msg)) {
            ## Always return a `stopifnot()`-like error.
            msg <- sprintf("Assert failure.\n[%s] %s is not TRUE.", i, call)
            ## Check for defined cause attribute.
            cause <- cause(r)
            if (!is.null(cause)) {
                ## Capturing the S3 print method on goalie class here.
                msg <- c(msg, capture.output(print(r))[-1L])
            }
            msg <- paste0(msg, collapse = "\n")
        }

        ## Include the traceback in error.
        if (isTRUE(traceback)) {
            ## Note that we're reversing the call stack here to make it
            ## easier to see the parents.
            stack <- rev(sys.calls())
            stack <- printString(stack)
            ## Add the traceback to the error message.
            msg <- paste(msg, "Traceback:", stack, sep = "\n")
        }

        stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
    }

    invisible(TRUE)
}
