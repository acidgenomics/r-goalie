#' Is Docker enabled (running) on the current machine?
#'
#' @name check-scalar-isDockerEnabled
#' @note Updated 2021-08-19.
#'
#' @inherit check return
#'
#' @examples
#' isDockerEnabled()
NULL



## nocov start

#' @rdname check-scalar-isDockerEnabled
#' @export
isDockerEnabled <- function() {
    ok <- isASystemCommand("docker")
    if (!isTRUE(ok)) {
        return(false("Docker is not installed."))
    }
    ok <- tryCatch(
        expr = {
            system2(
                command = "docker",
                args = "info",
                stdout = FALSE,
                stderr = FALSE
            )
        },
        warning = function(w) FALSE,
        error = function(e) FALSE
    )
    if (!isTRUE(ok)) {
        return(false("Docker is installed, but not enabled."))
    }
    TRUE
}

## nocov end
