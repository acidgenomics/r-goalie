#' Does the requested input exist in the environment?
#'
#' @note `exists` only supports `character(1)`, so we are exporting
#'   `isExisting` as a convenience function to check multiple variables in a
#'   single call.
#'
#' @name check-vector-isExisting
#' @inherit params
#'
#' @param x `character`.
#'   Variable names to check in `environment`.
#'
#' @seealso `assertive.code::is_existing()`.
#'
#' @examples
#' a <- 1L
#' b <- 2L
#'
#' ## TRUE ====
#' isExisting(c("a", "b"))
#' allAreNonExisting(c("x", "y"))
#'
#' ## FALSE ====
#' isExisting(c("x", "y"))
NULL



# Vector =======================================================================
#' @describeIn check-vector-isExisting Vectorized.
#' @export
# Updated 2019-07-15.
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



#' @describeIn check-vector-isExisting Vectorized.
#' @export
# Updated 2019-07-15.
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



# Scalar =======================================================================
#' @describeIn check-vector-isExisting Scalar.
#' @export
# Updated 2019-07-15.
allAreExisting <- function() {
    ok <- isExisting(
        x = x,
        envir = envir,
        inherits = inherits,
        .xname = .xname
    )
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreExisting) <- formals(isExisting)



#' @describeIn check-vector-isExisting Scalar.
#' @export
# Updated 2019-07-15.
allAreNonExisting <- function() {
    ok <- isNonExisting(
        x = x,
        envir = envir,
        inherits = inherits,
        .xname = .xname
    )
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreNonExisting) <- formals(isNonExisting)
