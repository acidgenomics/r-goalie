## nocov start



#' Is the current R session running inside RStudio?
#'
#' @name check-scalar-isRstudio
#' @note Updated 2020-04-12.
#'
#' @inherit check return
#'
#' @examples
#' isRstudio()
NULL



#' @rdname check-scalar-isRstudio
#' @export
isRstudio <- function() {
    ok <- isTRUE(nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY")))
    if (!isTRUE(ok)) {
        return(false("RStudio not detected."))
    }
    TRUE
}



## nocov end
