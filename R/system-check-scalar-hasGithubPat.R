#' Does the current session have a GitHub personal access token?
#'
#' Required for package installs from GitHub, otherwise will hit rate limit.
#'
#' @name check-scalar-hasGithubPat
#' @note Updated 2025-02-26.
#'
#' @inherit check return
#'
#' @examples
#' ## TRUE ====
#' Sys.setenv("GITHUB_PAT" = "XXX")
#' hasGithubPat()
#'
#' ## FALSE ====
#' Sys.setenv("GITHUB_PAT" = "")
#' hasGithubPat()
NULL



#' @rdname check-scalar-hasGithubPat
#' @export
hasGithubPat <- function() {
    ok <- isTRUE(nzchar(Sys.getenv("GITHUB_PAT")))
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} is not a defined environment variable.",
            "GITHUB_PAT"
        ))
    }
    TRUE
}
