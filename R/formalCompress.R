#' Check the "compress" Formal Argument
#'
#' @name formalCompress
#' @inherit params
#'
#' @param compress `logical(1)` or `character(1)`. These character strings are
#'   currently allowed for `save()`: `"gzip"`, `"bzip2"``, or `"xz"`.
#'
#' @examples
#' ## Pass ====
#' formalCompress("gzip")
#' formalCompress(TRUE)
#'
#' ## Fail ====
#' formalCompress("xxx")
NULL



.formalCompress <- function(compress) {
    if (!isAny(compress, classes = c("character", "logical"))) {
        return("Must contain character or logical.")
    }

    # Allow TRUE/FALSE boolean flag.
    if (is.logical(compress)) {
        if (!isFlag(compress)) {
            return("Logical input must contain boolean flag (TRUE/FALSE).")
        }
        return(compress)
    }

    choices <- c("bzip2", "gzip", "xz")
    if (!isSubset(compress, choices)) {
        return(paste("Valid strings:", toString(choices)))
    }

    TRUE
}



#' @rdname formalCompress
#' @export
formalCompress <- makeTestFunction(.formalCompress)
