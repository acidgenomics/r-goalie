#' Does the Input Contain an Alpha Level?
#'
#' An alpha level must be `numeric(1)` greater than 0 and less than 1.
#'
#' @name containsAlpha
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' containsAlpha(0.05)
#'
#' ## Fail ====
#' containsAlpha("xxx")
#' containsAlpha(1L)
NULL



.containsAlpha <- function(x) {
    msg <- "An alpha level must be a scalar numeric > 0 and < 1"
    ok <- isScalarDouble(x)
    if (!isTRUE(ok)) {
        return(msg)
    }
    ok <- x > 0L && x < 1L
    if (!isTRUE(ok)) {
        return(msg)
    }
    TRUE
}



#' @rdname containsAlpha
#' @export
containsAlpha <- makeTestFunction(.containsAlpha)
