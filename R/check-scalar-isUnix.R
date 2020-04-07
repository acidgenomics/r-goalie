#' Is the operating system Unix-based?
#'
#' @details
#' This check will return `TRUE` on Linux and macOS but `FALSE` on Windows.
#'
#' @name check-scalar-isUnix
#' @note Updated 2020-04-07.
#'
#' @inherit check return
#'
#' @examples
#' isUnix()
NULL



#' @rdname check-scalar-isUnix
#' @export
isUnix <- function() {
    identical(.Platform[["OS.type"]], "unix")
}
