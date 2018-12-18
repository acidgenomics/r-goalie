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
    ok <- isScalarDouble(x)
    if (!isTRUE(ok)) {
        return(false("%s must be scalar double.", .xname))
    }
    ok <- x > 0L && x < 1L
    if (!isTRUE(ok)) {
        return(false(paste0(
            "%s does not contain a valid alpha.\n",
            "Alpha level must be > 0 and < 1."
        ), .xname))
    }
    TRUE
}



# Soft-deprecate?
#' @rdname containsAlpha
#' @export
isAlpha <- containsAlpha
