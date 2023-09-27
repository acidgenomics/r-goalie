#' @describeIn check-scalar-isScalar Alias for [isScalarNumeric()].
#' @export
isNumber <- function(x, nullOk = FALSE) {
    if (isTRUE(nullOk) && is.null(x)) {
        return(TRUE)
    }
    isScalarNumeric(x)
}
