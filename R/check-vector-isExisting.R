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
#' allAreNonExisting(c("x", "y"))
#'
#' ## Fail ====
#' isExisting(c("x", "y"))
NULL



# vector =======================================================================
#' @describeIn isExisting Vectorized.
#' @export
isExisting <- function(
    x,
    envir = parent.frame(),
    inherits = FALSE,
    .xname = getNameInParent(x)
)  {
    ok <- isCharacter(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- bapply(
        X = x,
        FUN = exists,
        envir = envir,
        inherits = inherits
    )
    setCause(ok, false = "non-existing")
}



#' @describeIn isExisting Vectorized.
#' @export
isNonExisting <- function() {
    ok <- !isExisting(
        x = x,
        envir = envir,
        inherits = inherits,
        .xname = .xname
    )
    setCause(ok, false = "existing")
}

formals(isNonExisting) <- formals(isExisting)



# scalar =======================================================================
#' @describeIn isExisting Scalar.
#' @export
allAreExisting <- function() {
    ok <- isExisting(
        x = x,
        envir = envir,
        inherits = inherits,
        .xname = .xname
    )
    if (!all(ok)) return(ok)
    TRUE
}

formals(allAreExisting) <- formals(isExisting)



#' @describeIn isExisting Scalar.
#' @export
allAreNonExisting <- function() {
    ok <- isNonExisting(
        x = x,
        envir = envir,
        inherits = inherits,
        .xname = .xname
    )
    if (!all(ok)) return(ok)
    TRUE
}

formals(allAreNonExisting) <- formals(isNonExisting)
