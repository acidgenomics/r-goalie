#' Is the operating system Linux?
#'
#' @name check-scalar-isLinux
#' @note Updated 2020-04-07.
#'
#' @inherit check return
#'
#' @examples
#' isLinux()
NULL



#' @rdname check-scalar-isLinux
#' @export
isLinux <- function() {
    isTRUE(grepl(pattern = "linux", x = R.Version()[["os"]]))
}
