#' Does the current session have an internet connection?
#'
#' @note Requires curl package to be installed.
#'
#' @name check-scalar-hasInternet
#' @note Updated 2019-07-29.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @examples
#' hasInternet()
NULL



#' @rdname check-scalar-hasInternet
#' @export
hasInternet <- function() {
    requireNamespace("curl", quietly = TRUE)
    ok <- tryCatch(
        expr = curl::has_internet(),
        error = function(e) FALSE,
        warning = function(w) FALSE
    )
    if (!isTRUE(ok)) {
        return(false("Internet connection test failed."))  # nocov
    }
    TRUE
}
