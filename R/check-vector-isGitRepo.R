## nocov start



#' Does the input contain a Git repository?
#'
#' @name check-vector-isGitRepo
#' @note Updated 2021-08-19.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isGitRepo(file.path("~", "git", "monorepo"))
#'
#' ## FALSE ====
#' isGitRepo("~")
NULL



## Vector ======================================================================
#' @describeIn check-vector-isGitRepo Vectorized.
#' @export
isGitRepo <- function(x) {
    ok <- isCharacter(x)
    if (!all(ok)) return(ok)
    ok <- bapply(
        X = x,
        FUN = function(x) {
            ok <- isADirectory(x)
            if (!isTRUE(ok)) return(FALSE)
            ok <- isADirectory(file.path(x, ".git"))
            if (isTRUE(ok)) return(TRUE)
            ok <- isSystemCommand("git")
            if (!isTRUE(ok)) return(FALSE)
            wd <- getwd()
            setwd(x)
            ok <- tryCatch(
                expr = {
                    x <- system2(
                        command = "git",
                        args = c("rev-parse", "--git-dir"),
                        stdout = TRUE,
                        stderr = FALSE
                    )
                    isADir(x)
                },
                warning = function(w) FALSE,
                error = function(e) FALSE
            )
            setwd(wd)
            ok
        }
    )
    setCause(ok, false = "not git repo")
}



## Scalar ======================================================================
#' @describeIn check-vector-isGitRepo Scalar.
#' @export
isAGitRepo <- function(x) {
    ok <- isString(x)
    if (!isTRUE(ok)) return(ok)
    ok <- isGitRepo(x)
    if (!isTRUE(ok)) return(ok)
    TRUE
}

#' @describeIn check-vector-isGitRepo Scalar.
#' @export
allAreGitRepos <- function(x) {
    ok <- isGitRepo(x)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}



## nocov end
