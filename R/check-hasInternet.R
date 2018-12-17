#' Does the current session have an internet connection?
#' @inherit params
#' @export
#' @examples
#' hasInternet()
hasInternet <- function() {
    requireNamespace("curl", quietly = TRUE)
    curl::has_internet
}
