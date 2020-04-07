#' Is a system command installed?
#'
#' @name check-vector-isSystemCommand
#' @note Updated 2020-04-07.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @examples
#' isSystemCommand(which = "ls")
NULL



## Vector ======================================================================
#' @describeIn check-vector-isSystemCommand Vectorized.
#' @export
isSystemCommand <- function(x) {
    ok <- nzchar(Sys.which(x))
    names(ok) <- toNames(x)
    setCause(ok, false = "not command")
}



## Scalar ======================================================================
#' @describeIn check-vector-isMatching Scalar.
#' @export
allAreSystemCommands <- function(x) {
    ok <- isSystemCommand(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}