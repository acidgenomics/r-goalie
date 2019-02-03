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
    dots <- as.call(substitute(...()))

    # Note that here we're evaluating all of the checks instead of stopping on
    # the first error, like the approach in `assert()`.
    res <- lapply(
        X = seq_along(dots),
        FUN = function(i) {
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

            # Validity checks must return logical(1) or character(1).
            # In the event of FALSE, we'll return character(1) automatically.
            if (!(
                length(res) == 1L &&
                (is.logical(res) || is.character(res))
            )) {
                stop(paste(
                    "Validity check failure.",
                    "Checks must return logical(1) or character(1).",
                    .Dparse(call),
                    sep = "\n"
                ))
            }

            if (isTRUE(res)) {
                # Return TRUE if the validity check passed.
                return(TRUE)
            } else if (is.logical(res)) {
                # Convert an assert check error to a character string.
                # Always return a `stopifnot()`-like message.
                msg <- sprintf("%s is not TRUE.", .Dparse(call))
                # Check for defined cause attribute.
                cause <- cause(res)
                if (!is.null(cause)) {
                    # Capturing the S3 print method on goalie class here.
                    msg <- c(msg, capture.output(print(res))[-1L])
                }
                msg <- paste0(msg, collapse = "\n")
            } else if (is.character(res)) {
                # We're allowing the user to pass character(1) through here,
                # enabling the use of other check functions (see checkmate
                # package for examples).
                msg <- res
            }

            as.character(msg)
        }
    )

    if (all(bapply(res, isTRUE))) {
        # Return TRUE boolean flag when all checks pass.
        TRUE
    } else if (isString(msg)) {
        msg
    } else {
        # Otherwise, return a character string indicating which checks failed.
        # Note that we need to remove checks that return TRUE here.
        fail <- Filter(Negate(isTRUE), res)
        # Convert the list to a character vector.
        fail <- unlist(fail)
        # Return character string indicating all of the failures.
        # Using two line breaks here so we can visually distinguish checks
        # with a cause attribute set.
        paste0(fail, collapse = "\n\n")
    }
}
