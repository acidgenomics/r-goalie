#' @describeIn check-scalar-isScalar Alias for [isScalarNumeric()].
#' @export
# Updated 2019-07-15.
isNumber <- function(x, nullOK = FALSE) {
    if (isTRUE(nullOK) && is.null(x)) return(TRUE)
    isScalarNumeric(x)
}
