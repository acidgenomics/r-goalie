#' Is the operating system Windows?
#'
#' @name check-scalar-isWindows
#' @note Updated 2020-04-12.
#'
#' @inherit check return
#'
#' @examples
#' isWindows()
NULL



#' @rdname check-scalar-isWindows
#' @export
isWindows <- function() {
    ok <- identical(.Platform[["OS.type"]], "windows")
    if (!isTRUE(ok)) {
        return(false("Windows not detected."))
    }
    TRUE
}
