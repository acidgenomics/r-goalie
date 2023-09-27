#' Does the current session have a GitHub personal access token?
#'
#' Required for package installs from GitHub, otherwise will hit rate limit.
#'
#' @name check-scalar-hasGithubPat
#' @note Updated 2019-10-04.
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
    isTRUE(nzchar(Sys.getenv("GITHUB_PAT")))
}
