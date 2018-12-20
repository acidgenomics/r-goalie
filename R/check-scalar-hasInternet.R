#' Does the current session have an internet connection?
#'
#' @export
#' @inherit params
#'
#' @examples
#' hasInternet()
hasInternet <- function() {
    ok <- tryCatch(
        expr = curl::has_internet(),
        error = function(e) FALSE,
        warning = function(w) FALSE
    )
    if (!isTRUE(ok)) {
        return(false("Internet connection test failed."))
    }
    TRUE
}
