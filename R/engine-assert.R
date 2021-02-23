## FIXME THIS IS CAUSING THE GOALIE PRINT METHOD, WHICH HAS ISSUES WITH R MARKDOWN.
## FIXME NEED TO RETURN SCALAR CAUSE FROM S4 CLASS.



#' Assert that certain conditions are true
#'
#' [assert()] is a drop-in replacement for [`stopifnot()`][base::stopifnot]
#' supporting more informative error messages.
#'
#' If any of the expressions defined in `...` are not [`all`](base::all) `TRUE`,
#' [`stop`][base::stop] is called, producing an error message indicating the
#' first expression which was not `TRUE`.
#'
#' @name engine-assert
#' @note Updated 2021-02-23.
#'
#' @inheritParams AcidRoxygen::params
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
NULL



#' @rdname engine-assert
#' @export
assert <- function(..., msg = NULL) {
    n <- ...length()
    if (identical(n, 0L)) {
        stop("No assert check defined.")
    }
    dots <- as.call(substitute(...()))
    for (i in seq_len(n)) {
        r <- ...elt(i)
        if (!is(r, "goalie")) {
            r <- unname(r)
        }
        call <- .deparse(dots[[i]])
        if (!(is.logical(r) && identical(length(r), 1L))) {
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
                ## FIXME RETHINK THIS APPROACH...ISSUES WITH R MARKDOWN?
                msg <- c(msg, capture.output(print(r))[-1L])
            }
            msg <- paste0(msg, collapse = "\n")
        }
        stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
    }
    invisible(TRUE)
}
