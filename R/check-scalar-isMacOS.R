#' Is the operating system macOS?
#'
#' @name check-scalar-isMacOS
#' @note Updated 2020-04-07.
#'
#' @inherit check return
#'
#' @examples
#' isMacOS()
NULL



#' @rdname check-scalar-isMacOS
#' @export
isMacOS <- function() {
    grepl(pattern = "darwin", x = R.Version()[["os"]])
}
