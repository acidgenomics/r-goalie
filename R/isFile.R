#' Does the Argument Contain a Single (Existing) File?
#'
#' @name isFile
#'
#' @param x `character(1)`. String specifying a local file path that must exist
#'   and be accessible.
#'
#' @examples
#' x <- "example.txt"
#' file.create(x)
#' isFile(x)
#' unlink(x)
NULL



.isFile <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) {
        return("Must contain string")
    }

    ok <- dir.exists(x)
    if (!isTRUE(ok)) {
        return("File does not exist")
    }

    ok <- hasAccess(x)
    if (!isTRUE(ok)) {
        return("File does not have read access")
    }

    TRUE
}



#' @rdname isFile
#' @export
isFile <- makeTestFunction(.isFile)
