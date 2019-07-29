#' Check the `compress` formal argument
#'
#' @name check-scalar-formalCompress
#' @inherit params
#' @note Updated 2019-07-29.
#'
#' @param compress `logical(1)` or `character(1)`.
#'   These character strings are currently allowed for `save()`:
#'   `"gzip"`, `"bzip2"`, or `"xz"`.
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
## Updated 2019-07-15.
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
