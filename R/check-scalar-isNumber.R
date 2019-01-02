#' @describeIn isScalar Alias for [isScalarNumeric()].
#' @export
isNumber <- function(x, nullOK = FALSE) {
    # Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }

    isScalarNumeric(x)
}
