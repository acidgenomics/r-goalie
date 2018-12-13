#' Does the Input Contain a File?
#'
#' @name isFile
#' @importFrom R.utils isFile
#' @inherit params
#'
#' @return `logical`.
#'
#' @seealso `R.utils::isFile`.
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
    R.utils::isFile(x)
}



#' @describeIn isFile Check for a single file.
#' @export
isAFile <- function(x) {
    isString(x) && isFile(x)
}
