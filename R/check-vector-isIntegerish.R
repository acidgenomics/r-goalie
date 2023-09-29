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
        naOk = FALSE) {
    if (is(x, "Rle")) {
        requireNamespaces("S4Vectors")
        x <- S4Vectors::decode(x)
    }
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    xnames <- .toNames(x)
    ok <- is.numeric(x)
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        names(ko) <- xnames
        return(setCause(ko, false = "not numeric"))
    }
    if (isTRUE(naOk)) {
        x[is.na(x)] <- 0L
    }
    if (isTRUE(infiniteOk)) {
        x[is.infinite(x)] <- 0L
    }
    ok <- is.integer(x)
    if (all(ok)) {
        names(ok) <- xnames
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
    names(ok) <- xnames
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
