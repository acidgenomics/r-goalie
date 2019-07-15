#' Does the current session have an internet connection?
#'
#' @export
#' @inherit params
#'
#' @examples
#' hasInternet()

# Updated 2019-07-15.
hasInternet <- function() {
    requireNamespace("curl", quietly = TRUE)
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
