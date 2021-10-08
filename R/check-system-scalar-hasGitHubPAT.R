#' Does the current session have a GitHub personal access token?
#'
#' Required for package installs from GitHub, otherwise will hit rate limit.
#'
#' @name check-scalar-hasGitHubPAT
#' @note Updated 2019-10-04.
#'
#' @inherit check return
#'
#' @examples
#' ## TRUE ====
#' Sys.setenv("GITHUB_PAT" = "XXX")
#' hasGitHubPAT()
#'
#' ## FALSE ====
#' Sys.setenv("GITHUB_PAT" = "")
#' hasGitHubPAT()
NULL



#' @rdname check-scalar-hasGitHubPAT
#' @export
hasGitHubPAT <- function() {
    isTRUE(nzchar(Sys.getenv("GITHUB_PAT")))
}
