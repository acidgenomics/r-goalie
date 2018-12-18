#' Does the input contain an alpha level?
#'
#' An alpha level must be `numeric(1)` greater than 0 and less than 1.
#'
#' @name containsAlpha
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' containsAlpha(0.05)
#'
#' ## Fail ====
#' containsAlpha("xxx")
#' containsAlpha(1L)
containsAlpha <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalarDouble(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- isInClosedRange(x, lower = 0L, upper = 1L, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    TRUE
}



# Soft-deprecate?
#' @rdname containsAlpha
#' @export
isAlpha <- containsAlpha
