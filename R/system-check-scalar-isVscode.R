#' Is the current R session running inside Visual Studio Code?
#'
#' @name check-scalar-isVscode
#' @note Updated 2022-10-18.
#'
#' @inherit check return
#'
#' @examples
#' isVscode()
NULL



#' @rdname check-scalar-isVscode
#' @export
isVscode <- function() {
    ok <- isTRUE(nzchar(Sys.getenv("VSCODE_INIT_R")))
    if (!isTRUE(ok)) {
        return(false("Visual Studio Code not detected."))
    }
    TRUE
}
