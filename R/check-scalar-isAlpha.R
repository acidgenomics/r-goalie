#' Does the input contain an alpha level?
#'
#' An alpha level must be `numeric(1)` greater than 0 and less than 1.
#'
#' @name check-scalar-isAlpha
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isAlpha(0.05)
#'
#' ## FALSE ====
#' isAlpha("xxx")
#' isAlpha(1L)
NULL



#' @rdname check-scalar-isAlpha
#' @export
isAlpha <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalarDouble(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not scalar double.", .xname))
    }
    ok <- isInOpenRange(x, lower = 0L, upper = 1L, .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    TRUE
}
