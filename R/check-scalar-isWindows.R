#' Is the operating system Windows?
#'
#' @name check-scalar-isWindows
#' @note Updated 2020-04-07.
#'
#' @inherit check return
#'
#' @examples
#' isWindows()
NULL



#' @rdname check-scalar-isWindows
#' @export
isWindows <- function() {
    identical(.Platform[["OS.type"]], "windows")
}
