## nocov start



#' Is the current R session running inside Visual Studio Code?
#'
#' @name check-scalar-isVSCode
#' @note Updated 2022-10-18.
#'
#' @inherit check return
#'
#' @examples
#' isVSCode()
NULL



#' @rdname check-scalar-isVSCode
#' @export
isVSCode <- function() {
    ok <- isTRUE(nzchar(Sys.getenv("VSCODE_INIT_R")))
    if (!isTRUE(ok)) {
        return(false("Visual Studio Code not detected."))
    }
    TRUE
}



## nocov end
