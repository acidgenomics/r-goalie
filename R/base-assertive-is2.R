#' Is an object from a class?
#'
#' Alternate version of [methods::is()] that supports multiple checks in a
#' single call.
#'
#' If a function named `is.class` exists, call `is.class(x)`.
#' If not, call `is(x, class)`.
#'
#' @seealso
#' - `assertive.base::is2()`.
#' - `methods::is()`.
#'
#' @examples
#' is2(1:5, "character")
#' is2(matrix(1:5), "character")
#' is2(1:5, c("character", "list", "numeric"))
#' is2(mean, c("function", "data.frame"))
is2 <- function(x, class, .xname = getNameInParent(x)) {
    if (length(class) == 0L) {
        stop("You must provide a class.")
    }
    if (length(class) > 1L) {
        return(setCause(
            bapply(class, function(cl) is2(x, cl, "")),
            sprintf("%s is not '%s'", typeDescription(x), class)
        ))
    }
    # Attempt to use `is.character(x)` first.
    # Otherwise, fall back to `is(x, "character)`.
    # Alternatively, can take a look at `inherits()` but it doesn't always
    # return the same TRUE/FALSE as an `is()` call.
    ok <- tryCatch(
        expr = {
            is.class <- match.fun(paste0("is.", class))
            is.class(x)
        },
        error = function(e) {
            is(x, class)
        }
    )
    if (!isTRUE(ok)) {
        return(false(
            "%s is not of class '%s'; it has %s.",
            .xname,
            class,
            typeDescription(x)
        ))
    }
    TRUE
}
