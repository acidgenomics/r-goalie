## FIXME This should return TRUE for Rle.
## FIXME This should return TRUE for factor.
## FIXME This should return FALSE for matrix.



#' Is the input vector(ish)?
#'
#' @name check-scalar-isVectorish
#' @note Updated 2022-12-14.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isVectorish(c("aaa", "bbb", "ccc"))
#' isVectorish(c(1L, 2L, 3L))
#' isVectorish(c(TRUE, FALSE))
#'
#' ## FALSE ====
#' isVectorish(character())
#' isVectorish(matrix())
#' isVectorish(data.frame())
NULL



#' @rdname check-scalar-isVectorish
#' @export
isVectorish <-
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



        TRUE
    }
