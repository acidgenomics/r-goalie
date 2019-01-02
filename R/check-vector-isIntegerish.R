#' Is the input integer(ish)?
#'
#' Check for valid input of either explicit (e.g. `1L`) and/or implict
#' (e.g. `1`) `integer`.
#'
#' @name isIntegerish
#' @inherit params
#'
#' @seealso
#' - `isInt()` or `isScalarIntegerish()` for scalar.
#' - `rlang::is_integerish()`.
#' - `checkmate::checkIntegerish()`.
#'
#' @examples
#' isIntegerish(seq_len(2L))
#' isIntegerish(c(1, 2))
NULL



#' @describeIn isIntegerish Vectorized.
#' @export
isIntegerish <- function(x, .xname = getNameInParent(x)) {
    if (!is.numeric(x)) {
        return(false("%s is not numeric.", .xname))
    }
    if (any(is.na(x))) {
        return(false("%s contains NA.", .xname))
    }
    if (is.integer(x) || is.infinite(x)) {
        return(TRUE)
    }
    bapply(
        X = x,
        FUN = function(x) {
            isTRUE(all.equal(
                target = as.integer(x),
                current = x,
                tolerance = .tolerance
            ))
        }
    )
}



#' @describeIn isIntegerish Scalar.
#' @export
isInt <- function(x, nullOK = FALSE) {
    # Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    isScalarIntegerish(x)
}
