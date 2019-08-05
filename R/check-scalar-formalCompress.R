#' Check the `compress` formal argument
#'
#' @name check-scalar-formalCompress
#' @note Updated 2019-07-29.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @examples
#' ## TRUE ====
#' formalCompress("gzip")
#' formalCompress(TRUE)
#'
#' ## FALSE ====
#' formalCompress(NA)
#' formalCompress("xxx")
NULL



#' @rdname check-scalar-formalCompress
#' @export
formalCompress <- function(compress) {
    ok <- isAny(compress, classes = c("character", "logical"))
    if (!isTRUE(ok)) {
        return(ok)
    }

    ## Allow TRUE/FALSE boolean flag.
    if (is.logical(compress)) {
        ok <- isFlag(compress)
        if (!isTRUE(ok)) {
            return(ok)
        }
        return(compress)
    }

    ok <- isSubset(compress, c("bzip2", "gzip", "xz"))
    if (!isTRUE(ok)) {
        return(ok)
    }

    TRUE
}
