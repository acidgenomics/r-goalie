# FIXME Set the formals using `formals` here for consistency.



#' Does the requested input exist in the environment?
#'
#' @note `exists` only supports `character(1)`, so we are exporting
#'   `isExisting` as a convenience function to check multiple variables in a
#'   single call.
#'
#' @name isExisting
#' @inherit params
#'
#' @param x `character`.
#'   Variable names to check in `environment`.
#'
#' @seealso `assertive.code::is_existing()`.
#'
#' @examples
#' suppressWarnings(rm(x, y))
#' a <- 1L
#' b <- 2L
#'
#' ## Pass ====
#' isExisting(c("a", "b"))
#' areNonExisting(c("x", "y"))
#'
#' ## Fail ====
#' isExisting(c("x", "y"))
#' areNonExisting(c("a", "b"))
NULL



#' @rdname isExisting
#' @export
isExisting <- function(
    x,
    envir = parent.frame(),
    inherits = FALSE,
    .xname = getNameInParent(x)
)  {
    x <- as.character(x)

    ok <- isNonEmpty(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    # Allow parameterized return.
    if (length(x) > 1L) {
        return(bapply(x, isExisting, envir = envir, inherits = inherits))
    }

    if (!exists(x, envir = envir, inherits = inherits)) {
        return(false(gettext("%s does not exist."), .xname))
    }

    TRUE
}



#' @rdname isExisting
#' @export
areExisting <- function() {
    if (!length(x) > 1L) {
        stop("Use isExisting() for scalar.")
    }
    all(do.call(
        what = isExisting,
        args = list(
            x = x,
            envir = envir,
            inherits = inherits,
            .xname = .xname
        )
    ))
}

formals(areExisting) <- formals(isExisting)



# FIXME Rethink this approach...this won't return cause currently.
#' @rdname isExisting
#' @export
isNonExisting <- function() {
    as.logical(do.call(
        what = Negate(isExisting),
        args = list(
            x = x,
            envir = envir,
            inherits = inherits,
            .xname = .xname
        )
    ))
}

formals(isNonExisting) <- formals(isExisting)



#' @rdname isExisting
#' @export
areNonExisting <- function(x, ...) {
    if (!length(x) > 1L) {
        stop("Use isExisting() for scalar.")
    }
    all(do.call(
        what = isNonExisting,
        args = list(
            x = x,
            envir = envir,
            inherits = inherits,
            .xname = .xname
        )
    ))
}

formals(areNonExisting) <- formals(isNonExisting)
