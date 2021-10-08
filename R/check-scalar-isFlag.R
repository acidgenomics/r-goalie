#' Does the input contain a boolean flag?
#'
#' @name check-scalar-isFlag
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
        return(false(
            "{.var %s} is not a boolean flag ({.val %s}/{.val %s}).",
            .xname, "TRUE", "FALSE"
        ))
    }
    ## Check for `NA`, which is logical but not a flag.
    if (is.na(x)) {
        return(false("{.var %s} is {.val %s}.", .xname, "NA"))
    }
    TRUE
}
