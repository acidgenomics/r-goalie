#' Is the operating system macOS?
#'
#' @name check-scalar-isMacOS
#' @note Updated 2020-04-12.
#'
#' @inherit check return
#'
#' @examples
#' isMacOS()
NULL



#' @rdname check-scalar-isMacOS
#' @export
isMacOS <- function() {
    ok <- isTRUE(grepl(pattern = "darwin", x = R.Version()[["os"]]))
    if (!isTRUE(ok)) {
        return(false("macOS not detected."))
    }
    TRUE
}
