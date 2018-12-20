#' Validate an S4 class
#'
#' [validate()] is a variant of [assert()] that is specifically intended to be
#' used inside of an S4 validity method definition.
#'
#' Like [assert()], [validate()] returns `TRUE` on success. However, on failure
#' it returns a `character` instead of a [`stop()`][base::stop] call. This is
#' the current recommended practice for defining S4 validity methods inside of a
#' [`setValidity()`][methods::setValidity] call. Refer to the documentation in
#' the methods package, specifically [`validObject()`][methods::validObject] for
#' detailed information on S4 validity methods.
#'
#' @export
#' @inheritParams assert
#'
#' @seealso
#' - `methods::setValidity()`.
#' - `methods::validObject()`.
#' - `assertthat::validate_that()`.
#'
#' @examples
#' ## Pass ====
#' validate(
#'     is.atomic("example"),
#'     is.character("example")
#' )
#'
#' ## Fail ====
#' validate(
#'     isFlag("xxx"),
#'     isPositive(-1)
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
                # Always return a `stopifnot()`-like message.
                msg <- sprintf("%s is not TRUE.", .Dparse(call))
                # Check for defined cause attribute.
                cause <- cause(res)
                if (!is.null(cause)) {
                    # Capturing the S3 print method on goalie class here.
                    msg <- c(msg, capture.output(print(res))[-1L])
                }
                paste0(msg, collapse = "\n")
            } else {
                TRUE
            }
        }
    )

    # Return TRUE boolean flag when all checks pass.
    # Otherwise, return a character string indicating which checks failed.
    if (all(bapply(res, isTRUE))) {
        TRUE
    } else {
        paste0(unlist(res), collapse = "\n\n")
    }
}
