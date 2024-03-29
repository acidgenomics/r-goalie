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
isIntegerish <- function(x,
                         infiniteOk = TRUE,
                         naOk = FALSE) {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    if (is(x, "Rle")) {
        requireNamespaces("S4Vectors")
        x <- S4Vectors::decode(x)
    }
    ok <- is.numeric(x)
    if (!isTRUE(ok)) {
        ko <- rep(x = FALSE, times = length(x))
        return(setCause(ko, false = "not numeric"))
    }
    x[is.na(x)] <- ifelse(test = naOk, yes = 0L, no = 0.1)
    x[is.infinite(x)] <- ifelse(test = infiniteOk, yes = 0L, no = 0.1)
    ok <- is.integer(x)
    if (all(ok)) {
        return(rep(x = TRUE, times = length(x)))
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
