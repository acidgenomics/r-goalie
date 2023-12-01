#' Is the operating system macOS?
#'
#' @name check-scalar-isMacos
#' @note Updated 2020-04-12.
#'
#' @inherit check return
#'
#' @examples
#' isMacos()
NULL



#' @rdname check-scalar-isMacos
#' @export
isMacos <- function() {
    ok <- isTRUE(grepl(pattern = "darwin", x = R.Version()[["os"]]))

    if (!isTRUE(ok)) {
        return(false("macOS not detected."))
    }
    TRUE
}
