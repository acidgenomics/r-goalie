#' Does the input contain an existing (active) URL?
#'
#' @name check-vector-isExistingURL
#' @note Updated 2023-08-24.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `open.connection()`.
#' - `curl::has_internet()`.
#' - `curl::nslookup()`.
#' - `RCurl::getURL()`.
#' - https://stackoverflow.com/questions/52911812
#' - https://stackoverflow.com/a/17620732/3911732
#'
#' @examples
#' ## TRUE ====
#' isAnExistingURL("https://acidgenomics.com")
#'
#' ## FALSE ====
#' isAnExistingURL("https://failwhale.acidgenomics.com")
NULL



## Vector ======================================================================

#' @describeIn check-vector-isExistingURL Vectorized.
#' @export
isExistingURL <- function(x, .xname = getNameInParent(x)) {
    ok <- as.logical(capabilities(what = "http/ftp"))
    if (!isTRUE(ok)) {
        return(false("R session does not have Internet access."))
    }
    ok <- isURL(x, .xname = .xname)
    if (!all(ok)) {
        return(ok)
    }
    checkConnection <- function(x) {
        con <- url(x)
        test <- try(
            expr = {
                suppressWarnings({
                    open.connection(con = con, open = "rt", timeout = 2L)
                })
            },
            silent = TRUE
        )
        close(con)
        ok <- !inherits(test, "try-error")
        ok
    }
    ok <- bapply(X = x, FUN = checkConnection)
    setCause(ok, false = "URL doesn't exist")
}



## Scalar ======================================================================

#' @describeIn check-vector-isExistingURL Scalar. Requires a single URL.
#' @export
isAnExistingURL <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalar(x = x, .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isExistingURL(x = x, .xname = .xname)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isExistingURL Scalar. Checks that all strings are
#' existing URLs.
#' @export
allAreExistingURLs <- function(x, .xname = getNameInParent(x)) {
    ok <- isExistingURL(x = x, .xname = .xname)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
