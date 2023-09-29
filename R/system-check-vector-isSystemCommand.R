#' Is a system command installed?
#'
#' @name check-vector-isSystemCommand
#' @note Updated 2023-09-29.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isSystemCommand(c("cp", "rm"))
#'
#' ## FALSE ====
#' isSystemCommand(c("AAA", "BBB"))
NULL



## Vector ======================================================================

#' @describeIn check-vector-isSystemCommand Vectorized.
#' @export
isSystemCommand <- function(x) {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isCharacter(x)
    xnames <- .toNames(x)
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        names(ko) <- xnames
        return(setCause(ko, false = "not character"))
    }
    ok <- nzchar(Sys.which(x))
    names(ok) <- xnames
    setCause(ok, false = "not command")
}



## Scalar ======================================================================

#' @describeIn check-vector-isSystemCommand Scalar.
#' @export
isASystemCommand <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isSystemCommand(x)
    if (!all(ok)) {
        return(ok)
    }
    TRUE
}



#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreSystemCommands <- function(x) {
    ok <- isSystemCommand(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
