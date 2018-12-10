#' Does the Argument Contain a Directory?
#'
#' @name isDirectory
#' @importFrom R.utils isDirectory
#' @inherit params
#'
#' @seealso `R.utils::isDirectory()`.
#'
#' @examples
#' isADirectory("~")
NULL



#' @rdname isDirectory
#' @export
isDirectory <- function(x) {
    R.utils::isDirectory(x)
}



#' @rdname isDirectory
#' @export
isDir <- isDirectory



#' @rdname isDirectory
#' @export
isADirectory <- function(x) {
    isString(x) && isDirectory(x)
}



#' @rdname isDirectory
#' @export
isADir <- isADirectory
