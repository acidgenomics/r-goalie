#' Is the current R session running inside RStudio?
#'
#' @name check-scalar-isRStudio
#' @note Updated 2020-04-12.
#'
#' @inherit check return
#'
#' @examples
#' isRStudio()
NULL



#' @rdname check-scalar-isRStudio
#' @export
isRStudio <- function() {
    ok <- isTRUE(nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY")))
    ## nocov start
    if (!isTRUE(ok)) {
        return(false("RStudio not detected."))
    }
    TRUE
    ## nocov end
}
