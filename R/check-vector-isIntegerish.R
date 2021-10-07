## FIXME Add support for nullOK here.



#' Is the input integer(ish)?
#'
#' Check for valid input of either explicit (e.g. `1L`) and/or implict
#' (e.g. `1`) `integer`.
#'
#' @name check-vector-isIntegerish
#' @note Updated 2019-10-09.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `isInt()` or `isScalarIntegerish()` for scalar.
#' - `rlang::is_integerish()`.
#' - `checkmate::checkIntegerish()`.
#'
#' @examples
#' ## TRUE ====
#' isIntegerish(seq_len(2L))
#' isIntegerish(c(1, 2))
#'
#' ## FALSE ====
#' isIntegerish(0.1)
NULL



## Vector ======================================================================
#' @describeIn check-vector-isIntegerish Vectorized.
#' @export
isIntegerish <- function(x, .xname = getNameInParent(x)) {
    ## Check for numeric vector.
    if (!is.numeric(x)) {
        return(false("'%s' is not numeric.", .xname))
    }
    ## Require that vector does not contain NA.
    ok <- !is.na(x)
    if (!all(ok)) {
        names(ok) <- .toNames(x)
        return(setCause(ok, false = "NA"))
    }
    ## Early return without running `all.equal()` for integer or infinite (Inf).
    ok <- bapply(
        X = x,
        FUN = function(x) {
            is.integer(x) || is.infinite(x)
        }
    )
    names(ok) <- .toNames(x)
    ## Check if numeric is equal to integer, based on tolerance.
    if (all(ok)) {
        return(ok)
    }
    ok <- bapply(
        X = x,
        FUN = function(x) {
            isTRUE(all.equal(
                target = as.integer(x),
                current = x,
                tolerance = .tolerance
            ))
        }
    )
    names(ok) <- .toNames(x)
    setCause(ok, false = "not integer")
}



## Scalar ======================================================================
#' @describeIn check-vector-isIntegerish Scalar.
#' @export
isInt <- function(x, nullOK = FALSE) {
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }
    isScalarIntegerish(x)
}
