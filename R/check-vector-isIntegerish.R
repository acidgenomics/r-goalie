## FIXME The logic isn't quite right here using early returns, need to rethink.



#' Is the input integer(ish)?
#'
#' Check for valid input of either explicit (e.g. `1L`) and/or implict
#' (e.g. `1`) `integer`.
#'
#' @name check-vector-isIntegerish
#' @note Updated 2023-09-29.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param infiniteOk `logical(1)`.
#' Allow infinite values (`Inf` or `-Inf`) to pass.
#'
#' @param naOk `logical(1)`.
#' Allow `NA` values to pass.
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
#' isIntegerish(S4Vectors::Rle(c(1, 2)))
#'
#' ## FALSE ====
#' isIntegerish(0.1)
NULL



## Vector ======================================================================

#' @describeIn check-vector-isIntegerish Vectorized.
#' @export
isIntegerish <- function(
        x,
        infiniteOk = TRUE,
        naOk = FALSE,
        .xname = getNameInParent(x)) {
    if (is(x, "Rle")) {
        requireNamespaces("S4Vectors")
        x <- S4Vectors::decode(x)
    }
    ok <- is.numeric(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not numeric.", .xname))
    }
    if (isFALSE(naOk)) {
        ok <- !is.na(x)
        if (any(!ok)) {
            names(ok) <- .toNames(x)
            return(setCause(ok, false = "NA"))
        }
    }
    if (isFALSE(infiniteOk)) {
        ok <- !is.infinite(x)
        if (any(!ok)) {
            names(ok) <- .toNames(x)
            return(setCause(ok, false = "infinite"))
        }
    }
    ok <- bapply(
        X = x,
        FUN = function(x) {
            is.integer(x) || is.infinite(x)
        }
    )
    names(ok) <- .toNames(x)
    if (all(ok)) {
        return(ok)
    }
    ok <- bapply(
        X = x,
        FUN = function(x) {
            if (is.infinite(x) || is.na(x)) {
                return(TRUE)
            }
            all.equal(
                target = as.integer(x),
                current = x,
                tolerance = .tolerance
            )
        }
    )
    names(ok) <- .toNames(x)
    setCause(ok, false = "not integer")
}



## Scalar ======================================================================

#' @describeIn check-vector-isIntegerish Scalar.
#' @export
## Updated 2023-09-19.
allAreIntegerish <- function(x) {
    ok <- isIntegerish(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isIntegerish Scalar.
#' @export
isInt <- function(x, nullOk = FALSE) {
    if (isTRUE(nullOk) && is.null(x)) {
        return(TRUE)
    }
    isScalarIntegerish(x)
}
