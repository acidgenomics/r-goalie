#' Does the input contain a directory?
#'
#' @name isDirectory
#' @inherit params
#'
#' @return `logical`.
#'
#' @seealso
#' `R.utils::isDirectory()`.
#'
#' ```
#' getS3method(
#'     f = "isDirectory",
#'     class = "default",
#'     envir = asNamespace("R.utils")
#' )
#' ```
#'
#' @examples
#' isDirectory(c("~", "~"))
#' isADirectory("~")
NULL



#' @rdname isDirectory
#' @export
isDirectory <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) return(ok)

    dir.exists(x)
}



#' @rdname isDirectory
#' @export
isDir <- isDirectory



#' @rdname isDirectory
#' @export
isADirectory <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)

    ok <- isDirectory(x)
    if (!isTRUE(ok)) return(ok)

    TRUE
}



#' @rdname isDirectory
#' @export
isADir <- isADirectory
