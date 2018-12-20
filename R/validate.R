# TODO Include the assert check function name in the call if possible.



#' Validate an S4 class
#'
#' [validate()] is a variant of [assert()] that is specifically intended to be
#' used inside of an S4 validity method definition.
#'
#' Like [assert()], [validate()] returns `TRUE` on success. However, on failure
#' it returns a `character` instead of a [stop()][base::stop] call. This is the
#' current recommended practice for defining S4 validity methods inside of a
#' [setValidity()][methods::setValidity] call. Refer to the documentation in the
#' methods package, specifically [validObject()][methods::validObject] for
#' detailed information on S4 validity methods.
#'
#' @inheritParams assertthat::validate_that
#' @export
#'
#' @seealso
#' - `methods::setValidity()`.
#' - `methods::validObject()`.
#' - `assertthat::validate_that()`.
#'
#' @examples
#' validate(
#'     is.atomic("example"),
#'     is.character("example")
#' )
validate <- function(...) {
    mc <- match.call()[-1L]

    # Note that here we're evaluating all of the checks instead of stopping on
    # the first error, like the approach in `assert()`.
    res <- lapply(
        X = seq_along(mc),
        FUN = function(i) {
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

            # Add automatic `stopifnot()`-like cause attribute, if necessary.
            if (
                !isTRUE(res) &&
                identical(cause(res), noquote(""))
            ) {
                cause(res) <- sprintf("%s is not TRUE", deparse(call))
            }

            res
        }
    )

    # Return TRUE when all checks pass.
    if (all(vapply(
        X = res,
        FUN = isTRUE,
        FUN.VALUE = logical(1L)
    ))) {
        return(TRUE)
    }

    # Otherwise, return a character string indicating what checks failed.
    paste(
        vapply(
            X = Filter(f = Negate(isTRUE), x = res),
            FUN = cause,
            FUN.VALUE = character(1L)
        ),
        collapse = "\n"
    )
}
