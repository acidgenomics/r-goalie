#' Does the current session have an internet connection?
#'
#' @name check-scalar-hasInternet
#' @note Updated 2019-09-06.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @seealso
#' - `Biobase::testBioCConnection()`.
#' - `curl::has_internet()`.
#' - `curl::nslookup()`.
#' - `RCurl::getURL()`.
#' - https://stackoverflow.com/a/17620732/3911732
#'
#' @examples
#' hasInternet()
NULL



#' @rdname check-scalar-hasInternet
#' @export
hasInternet <- function(url = "http://www.bioconductor.org") {
    fail <- false("Internet connection test failed.")
    ok <- as.logical(capabilities(what = "http/ftp"))
    if (!isTRUE(ok)) {
        return(fail)  # nocov
    }
    url <- url(url)
    test <- try(
        expr = suppressWarnings(readLines(url, n = 1L)),
        silent = TRUE
    )
    if (inherits(test, "try-error")) {
        return(fail)  # nocov
    } else {
        close(url)
    }
    TRUE
}
