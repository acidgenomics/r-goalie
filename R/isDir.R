#' Does the Argument Contain a Single (Existing) Directory?
#'
#' @name isDir
#'
#' @param x `character(1)`. String specifying a local directory path that
#'   must exist and be accessible.
#'
#' @examples
#' isDir("~")
NULL



.isDir <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) {
        return("Must contain string")
    }

    ok <- dir.exists(x)
    if (!isTRUE(ok)) {
        return("Directory does not exist")
    }

    ok <- hasAccess(x)
    if (!isTRUE(ok)) {
        return("Directory does not have read access")
    }

    TRUE
}



#' @rdname isDir
#' @export
isDir <- makeTestFunction(.isDir)
