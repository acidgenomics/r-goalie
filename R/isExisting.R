# FIXME Set the formals using `formals()` here for consistency.



#' Does the Requested Input Exist in the Environment?
#'
#' @note `exists()` only supports `character(1)`, so we are exporting
#'   `isExisting()` as a convenience function to check multiple variables in a
#'   single call.
#'
#' @name isExisting
#' @importFrom assertive.code is_existing
#' @inherit params
#'
#' @param x `character`.
#'   Variable names to check in `environment`.
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
isExisting <- is_existing



#' @rdname isExisting
#' @export
areExisting <- function(x, ...) {
    all(isExisting(x, ...))
}



#' @rdname isExisting
#' @export
isNonExisting <- function(x, ...) {
    !isExisting(x, ...)
}



.areNonExisting <- function(x, ...) {
    ok <- !isExisting(x, ...)
    if (!all(ok)) {
        which <- names(ok)[!ok]
        return(paste("Exists in environment:", which))
    }
    TRUE
}



#' @rdname isExisting
#' @export
areNonExisting <- makeTestFunction(.areNonExisting)
