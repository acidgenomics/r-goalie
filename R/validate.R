## FIXME Need to add support for named arguments.



#' Validate an S4 class
#'
#' `validate()` is a variant of `assert()` that is specifically intended to be
#' used inside of an S4 validity method definition.
#'
#' Like `assert()`, `validate()` returns `TRUE` on success. However, on failure
#' it returns a `character` instead of a `stop()` call. This is the current
#' recommended practice for defining S4 validity methods inside of a
#' `setValidity()` call. Refer to the documentation in the methods package,
#' specifically `validObject()` for detailed information on S4 validity methods.
#'
#' @export
#' @note Updated 2021-10-08.
#'
#' @inheritParams assert
#'
#' @return `TRUE` on success, or `character(1)` on failure, containing an error
#'   message.
#'
#' @seealso
#' - `methods::setValidity()`.
#' - `methods::validObject()`.
#' - `assertthat::validate_that()`.
#'
#' @examples
#' ## TRUE ====
#' validate(
#'     is.atomic("example"),
#'     is.character("example")
#' )
#'
#' ## FALSE ====
#' validate(
#'     isFlag("xxx"),
#'     isPositive(-1)
#' )
validate <- function(..., msg = NULL) {
    n <- ...length()
    if (identical(n, 0L)) {
        stop("No validate check is defined.")
    }
    dots <- as.call(substitute(...()))
    ## Support character passthrough.
    if (length(dots) == 1L && is.character(dots[[1L]])) {
        return(dots[[1L]])
    }
    ## Note that here we're evaluating all of the checks instead of stopping on
    ## the first error, like the approach in `assert()`.
    checks <- lapply(
        X = seq_along(dots),
        FUN = function(i) {
            r <- ...elt(i)
            if (length(r) != 1L) {
                stop("Invalid input to validate.")
            }
            if (!is(r, "goalie")) {
                r <- unname(r)
            }
            call <- .deparse(dots[[i]])
            ## Validity checks must return `logical(1)` or `character(1)`.
            ## In the event of `FALSE`, return `character(1)` automatically.
            if (isTRUE(r)) {
                return(TRUE)
            } else if (is.character(r)) {
                ## We're allowing the user to pass `character(1)` through here,
                ## enabling the use of other check functions (see checkmate
                ## package for examples).
                msg <- r
            } else if (is.logical(r)) {
                ## Convert an assert check error to a character string.
                msg <- sprintf("[%s] %s is not TRUE.", i, call)
                if (is(r, "goalie")) {
                    cause <- cause(r)
                    if (!is.null(names(cause))) {
                        cause <- paste(names(cause), cause, sep = ": ")
                    }
                    msg <- paste0(msg, "\nCause: ", cause)
                }
            } else {
                stop(sprintf(
                    paste0(
                        "Validity failure.\n",
                        "Check did not return logical(1) or character(1).\n",
                        "[%s]: %s"
                    ),
                    i, call
                ))
            }
            as.character(msg)
        }
    )
    ## Return.
    if (all(bapply(X = checks, FUN = isTRUE))) {
        return(TRUE)
    }
    if (is.null(msg)) {
        fail <- unlist(Filter(f = Negate(isTRUE), x = checks))
        msg <- paste0(fail, collapse = "\n")
        msg <- paste0(
            msg, "\n",
            "If supported, {.fun updateObject} ",
            "may help resolve these issues."
        )
    }
    msg <- gsub(pattern = .cliPattern, replacement = "'\\1'", x = msg)
    msg
}
