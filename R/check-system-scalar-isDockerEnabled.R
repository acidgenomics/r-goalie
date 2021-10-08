#' Is Docker enabled (running) on the current machine?
#'
#' @name check-scalar-isDockerEnabled
#' @note Updated 2021-09-21.
#'
#' @inherit check return
#'
#' @examples
#' isDockerEnabled()
NULL



#' @rdname check-scalar-isDockerEnabled
#' @export
isDockerEnabled <- function() {
    ok <- isASystemCommand("docker")
    if (!isTRUE(ok)) {
        return(false("Docker is not installed."))
    }
    ok <- tryCatch(
        expr = {
            status <- system2(
                command = "docker",
                args = "info",
                stdout = FALSE,
                stderr = FALSE
            )
            identical(status, 0L)
        },
        warning = function(w) FALSE,
        error = function(e) FALSE
    )
    if (!isTRUE(ok)) {
        return(false("Docker is installed, but not enabled."))
    }
    TRUE
}
