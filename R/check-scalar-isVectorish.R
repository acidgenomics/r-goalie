#' Is the input vector(ish)?
#'
#' @details
#' Intentionally returns `TRUE` for some other vector-like classes, including
#' `factor`, and `Rle`.
#'
#' @name check-scalar-isVectorish
#' @note Updated 2022-12-14.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isVectorish(character())
#' isVectorish(factor())
#' isVectorish(integer())
#' isVectorish(logical())
#' isVectorish(S4Vectors::Rle())
#'
#' ## FALSE ====
#' isVectorish(data.frame())
#' isVectorish(matrix())
#' isVectorish(S4Vectors::DataFrame())
NULL



#' @rdname check-scalar-isVectorish
#' @export
isVectorish <-
    function(x,
             nullOk = FALSE) {
        if (isTRUE(nullOk) && is.null(x)) {
            return(TRUE)
        }
        if (is.factor(x) || is(x, "Rle")) {
            return(TRUE)
        }
        ok <- is.vector(x)
        if (!isTRUE(ok)) {
            return(false("{.var %s} is not a vector.", .toName(x)))
        }
        TRUE
    }
