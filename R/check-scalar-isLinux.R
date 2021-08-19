## nocov start



#' Is the operating system Linux?
#'
#' @name check-scalar-isLinux
#' @note Updated 2020-04-12.
#'
#' @inherit check return
#'
#' @examples
#' isLinux()
NULL



#' @rdname check-scalar-isLinux
#' @export
isLinux <- function() {
    ok <- isTRUE(grepl(pattern = "linux", x = R.Version()[["os"]]))
    if (!isTRUE(ok)) {
        return(false("Linux not detected."))
    }
    TRUE
}



## nocov end
