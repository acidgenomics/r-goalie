#' Does the current session have an internet connection?
#'
#' @export
#' @inherit params
#'
#' @examples
#' hasInternet()
hasInternet <- function() {
    requireNamespace("curl", quietly = TRUE)
    curl::has_internet()
}
