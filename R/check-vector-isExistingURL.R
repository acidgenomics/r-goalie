#' Does the input contain an existing (active) URL?
#'
#' @name check-vector-isExistingURL
#' @note Updated 2023-09-15.
#'
#' @details
#' Requires RCurl package to be installed.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `open.connection()`: Base method with no dependencies, but prone to hang
#' with poor timeout control.
#' - `RCurl::url.exists()`.
#' - `curl::has_internet()`.
#' - `curl::nslookup()`.
#' - https://stackoverflow.com/questions/52911812
#' - https://stackoverflow.com/a/17620732/3911732
#'
#' @examples
#' ## TRUE ====
#' isAnExistingURL("https://acidgenomics.com/")
#'
#' ## FALSE ====
#' isAnExistingURL("https://failwhale.acidgenomics.com/")
NULL



## Vector ======================================================================

#' @describeIn check-vector-isExistingURL Vectorized.
#' @export
isExistingURL <- function(x) {
    ok <- as.logical(capabilities(what = "http/ftp"))
    if (!isTRUE(ok)) {
        return(false("R session does not have Internet access."))
    }
    ok <- isURL(x)
    if (!all(ok)) {
        return(ok)
    }
    if (is(x, "url")) {
        checkConnection <- function(x) {
            test <- try(
                expr = {
                    suppressWarnings({
                        open.connection(con = con, open = "rt", timeout = 1L)
                    })
                },
                silent = TRUE
            )
            ok <- !inherits(test, "try-error")
        }
        ok <- checkConnection(x)
    } else {
        assert(requireNamespaces("RCurl"))
        ok <- RCurl::url.exists(x)
    }
    names(ok) <- x
    setCause(ok, false = "URL doesn't exist")
}



## Scalar ======================================================================

#' @describeIn check-vector-isExistingURL Scalar. Requires a single URL.
#' @export
isAnExistingURL <- function(x) {
    ok <- isScalar(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isExistingURL(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isExistingURL Scalar. Checks that all strings are
#' existing URLs.
#' @export
allAreExistingURLs <- function(x) {
    ok <- isExistingURL(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
