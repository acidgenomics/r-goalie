## nocov start



#' Is the operating system Unix-based?
#'
#' @details
#' This check will return `TRUE` on Linux and macOS but `FALSE` on Windows.
#'
#' @name check-scalar-isUnix
#' @note Updated 2020-04-12.
#'
#' @inherit check return
#'
#' @examples
#' isUnix()
NULL



#' @rdname check-scalar-isUnix
#' @export
isUnix <- function() {
    ok <- identical(.Platform[["OS.type"]], "unix")
    if (!isTRUE(ok)) {
        return(false("Unix not detected."))
    }
    TRUE
}



## nocov end
