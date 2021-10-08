## FIXME Need to add support for named arguments.



#' Assert that certain conditions are true
#'
#' `assert()` is a drop-in replacement for `stopifnot()` that supports more
#' informative error messages.
#'
#' If any of the expressions defined in `...` are `TRUE`, `stop()` is called,
#' producing an error message indicating the first expression which was not
#' `TRUE`.
#'
#' @export
#' @note Updated 2021-10-08.
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
assert <- function(..., msg = NULL) {
    hasCLI <- isInstalled("AcidCLI")
    if (isTRUE(hasCLI)) {
        stop <- AcidCLI::abort
    }
    n <- ...length()
    if (identical(n, 0L)) {
        stop("No assert check is defined.")
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
        if (is.null(msg)) {
            msg <- sprintf("Assert failure.\n[%s] %s is not TRUE.", i, call)
            if (is(r, "goalie")) {
                cause <- cause(r)
                if (!is.null(names(cause))) {
                    cause <- paste(names(cause), cause, sep = ": ")
                }
                msg <- paste0(msg, "\nCause: ", cause)
            }
        }
        ## Simplify CLI-formatted messages when AcidCLI is not installed.
        if (isFALSE(hasCLI)) {
            msg <- gsub(pattern = .cliPattern, replacement = "'\\1'", x = msg)
        }
        stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
    }
    invisible(TRUE)
}
