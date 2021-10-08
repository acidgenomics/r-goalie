#' Is conda enabled (active) in the current R session?
#'
#' @name check-scalar-isCondaEnabled
#' @note Updated 2021-08-19.
#'
#' @inherit check return
#'
#' @details
#' This checks `CONDA_DEFAULT_ENV` and `CONDA_SHLVL` system environment
#' variables internally.
#'
#' @param ignoreBase `logical(1)`.
#'   If `TRUE`, don't consider conda to be "active" if only the base
#'   environment is loaded. This is useful to avoid some false-positive
#'   situations when handling the main conda `activate` script in shell
#'   sessions.
#'
#' @examples
#' vars <- c("CONDA_DEFAULT_ENV", "CONDA_SHLVL")
#' Sys.unsetenv(vars)
#'
#' ## TRUE ====
#' Sys.setenv(
#'     "CONDA_DEFAULT_ENV" = "test",
#'     "CONDA_SHLVL" = "2"
#' )
#' isCondaEnabled()
#' Sys.unsetenv(vars)
#'
#' Sys.setenv(
#'     "CONDA_DEFAULT_ENV" = "base",
#'     "CONDA_SHLVL" = "1"
#' )
#' isCondaEnabled(ignoreBase = FALSE)
#' Sys.unsetenv(vars)
#'
#' ## FALSE ====
#' isCondaEnabled()
#'
#' Sys.setenv(
#'     "CONDA_DEFAULT_ENV" = "base",
#'     "CONDA_SHLVL" = "1"
#' )
#' isCondaEnabled(ignoreBase = TRUE)
#' Sys.unsetenv(vars)
NULL



#' @rdname check-scalar-isCondaEnabled
#' @export
isCondaEnabled <- function(ignoreBase = TRUE) {
    defaultEnv <- Sys.getenv("CONDA_DEFAULT_ENV")
    shlvl <- as.integer(Sys.getenv("CONDA_SHLVL"))
    ## Allow the user to selectively ignore when "base" environment is active.
    if (isTRUE(ignoreBase)) {
        ok <- identical(defaultEnv, "base")
        if (isTRUE(ok)) {
            return(false(
                "Ignoring active conda {.val %s} environment.",
                "base"
            ))
        }
    }
    ok <- isString(defaultEnv) && isTRUE(shlvl > 0L)
    if (!isTRUE(ok)) {
        return(false("Conda is not enabled."))
    }
    TRUE
}
