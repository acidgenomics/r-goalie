#' @describeIn check-scalar-isScalar Alias for [isScalarNumeric()].
#' @export
isNumber <- function(x, nullOK = FALSE) {
    if (isTRUE(nullOK) && is.null(x)) return(TRUE)
    isScalarNumeric(x)
}
