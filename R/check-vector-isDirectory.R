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



#' @describeIn isDirectory Vectorized.
#' @export
isDirectory <- function(x) {
    dir.exists(x)
}



#' @describeIn isDirectory Short alias for [isDirectory()].
#' @export
isDir <- isDirectory



#' @describeIn isDirectory Scalar.
#' @export
isADirectory <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)

    ok <- isDirectory(x)
    if (!isTRUE(ok)) return(ok)

    TRUE
}



#' @describeIn isDirectory Short alias for [isADirectory()].
#' @export
isADir <- isADirectory
