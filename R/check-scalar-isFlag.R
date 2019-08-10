#' Does the input contain a boolean flag?
#'
#' @name check-scalar-isFlag
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @examples
#' ## TRUE ====
#' isFlag(TRUE)
#' isFlag(FALSE)
#'
#' ## FALSE ====
#' isFlag(c(TRUE, TRUE))
#' isFlag(1)
#' isFlag(NA)
NULL



#' @rdname check-scalar-isFlag
#' @export
isFlag <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalarLogical(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not a boolean flag (TRUE/FALSE).", .xname))
    }
    ## Check for NA, which is logical but not a flag.
    if (is.na(x)) {
        return(false("'%s' is NA.", .xname))
    }
    TRUE
}
