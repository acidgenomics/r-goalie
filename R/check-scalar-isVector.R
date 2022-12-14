#' Does the input contain a (non-empty) vector?
#'
#' @name check-scalar-isVector
#' @note Updated 2022-12-14.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isVector(c("aaa", "bbb", "ccc"))
#' isVector(c(1L, 2L, 3L))
#' isVector(c(TRUE, FALSE))
#'
#' ## FALSE ====
#' isVector(character())
#' isVector(matrix())
#' isVector(data.frame())
#' isVector("")
#' isVector(NA)
NULL



#' @rdname check-scalar-isVector
#' @export
isVector <-
    function(x,
             nullOK = FALSE,
             .xname = getNameInParent(x)) {
        if (isTRUE(nullOK) && is.null(x)) {
            return(TRUE)
        }
        ok <- is.vector(x)
        if (!isTRUE(ok)) {
            return(false("{.var %s} is not a vector.", .xname))
        }
        ok <- !all(is.na(x))
        if (!isTRUE(ok)) {
            return(false("{.var %s} contains only {.val %s}.", .xname, "NA"))
        }
        ok <- !identical(x, "")
        if (!isTRUE(ok)) {
            return(false("{.var %s} contains empty string.", .xname))
        }
        TRUE
    }
