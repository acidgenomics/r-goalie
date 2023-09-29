#' Does the requested input exist in the environment?
#'
#' @note [`exists()`][base::exists] only supports `character(1)`, so we are
#' exporting [isExisting()] as a convenience function to check multiple
#' variables in a single call.
#'
#' @name check-vector-isExisting
#' @note Updated 2022-12-14.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param x `character`.
#' Variable names to check in `environment`.
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



## Vector ======================================================================

#' @describeIn check-vector-isExisting Vectorized.
#' @export
## Updated 2023-09-29.
isExisting <-
    function(x,
             envir = parent.frame(),
             inherits = FALSE,
             .xname = getNameInParent(x)) {
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
## Updated 2022-05-13.
isNonExisting <-
    function(x,
             envir = parent.frame(),
             inherits = FALSE,
             .xname = getNameInParent(x)) {
        ok <- !isExisting(
            x = x,
            envir = envir,
            inherits = inherits,
            .xname = .xname
        )
        setCause(ok, false = "existing")
    }



## Scalar ======================================================================

#' @describeIn check-vector-isExisting Scalar.
#' @export
## Updated 2022-05-13.
allAreExisting <-
    function(x,
             envir = parent.frame(),
             inherits = FALSE,
             .xname = getNameInParent(x)) {
        ok <- isExisting(
            x = x,
            envir = envir,
            inherits = inherits,
            .xname = .xname
        )
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }



#' @describeIn check-vector-isExisting Scalar.
#' @export
## Updated 2022-05-13.
allAreNonExisting <-
    function(x,
             envir = parent.frame(),
             inherits = FALSE,
             .xname = getNameInParent(x)) {
        ok <- isNonExisting(
            x = x,
            envir = envir,
            inherits = inherits,
            .xname = .xname
        )
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }
