#' Does the input contain a file?
#'
#' @name isFile
#' @inherit params
#'
#' @return `logical`.
#'
#' @seealso `R.utils::isFile()`.
#'
#' ```
#' getS3method(
#'     f = "isFile",
#'     class = "default",
#'     envir = asNamespace("R.utils")
#' )
#' ```
#'
#' @examples
#' x <- "example.txt"
#' file.create(x)
#' isAFile(x)
#' unlink(x)
NULL



#' @describeIn isFile Supports multiple files.
#' @export
isFile <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    file.exists(x)
}



#' @describeIn isFile Check for a single file.
#' @export
isAFile <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)

    ok <- isFile(x)
    if (!isTRUE(ok)) return(ok)

    TRUE
}
