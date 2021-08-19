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



#' @rdname check-scalar-isDockerEnabled
#' @export
isDockerEnabled <- function() {
    ok <- isSystemCommand("docker")
    if (!isTRUE(ok)) return(ok)
    ok <- tryCatch(
        expr = {
            ## FIXME Need to migrate this to system2 instead.
            shell(
                command = "docker",
                args = "info",
                print = FALSE
            )
        },
        error = function(e) {
            FALSE
        }
    )
    if (!isTRUE(ok)) return(ok)
    TRUE
}
