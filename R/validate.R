#' Validate an S4 Class
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
validate <- function(..., envir = parent.frame()) {
    res <- seeIf(..., envir = envir)
    stopifnot(is.list(res))

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
        "Validity check failure.",
        paste(
            vapply(
                X = Filter(f = Negate(isTRUE), x = res),
                FUN = cause,
                FUN.VALUE = character(1L)
            ),
            collapse = "\n"
        ),
        sep = "\n"
    )
}
