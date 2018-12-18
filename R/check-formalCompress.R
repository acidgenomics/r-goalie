# FIXME NA is breaking this...



#' Check the `compress` formal argument
#'
#' @name formalCompress
#' @inherit params
#' @export
#'
#' @param compress `logical(1)` or `character(1)`.
#'   These character strings are currently allowed for `save()`:
#'   `"gzip"`, `"bzip2"`, or `"xz"`.
#'
#' @examples
#' ## Pass ====
#' formalCompress("gzip")
#' formalCompress(TRUE)
#'
#' ## Fail ====
#' formalCompress(NA)
#' formalCompress("xxx")
formalCompress <- function(compress) {
    if (!isAny(compress, classes = c("character", "logical"))) {
        return(false("%s does not contain character or logical.", compress))
    }

    # Allow TRUE/FALSE boolean flag.
    if (is.logical(compress)) {
        if (!isFlag(compress)) {
            return(false("%s is logical but not boolean.", compress))
        }
        return(compress)
    }

    choices <- c("bzip2", "gzip", "xz")
    if (!isSubset(compress, choices)) {
        return(false(
            "%s is not a valid format. Supported choices: %s",
            compress, toString(choices)
        ))
    }

    TRUE
}
