## Here's a forbidden example:
## http://ftp.wormbase.org/
## sftp://sftp-private.ncbi.nlm.nih.gov/
##
## Here's a successful example:
## ftp://ftp.ensembl.org/pub/release-110/mysql/
## ftp://ftp.wormbase.org/



#' Does the input contain an existing (active) URL?
#'
#' @name check-vector-isExistingURL
#' @note Updated 2023-09-15.
#'
#' @details
#' Supports HTTPS, HTTP, and FTP protocols.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `open.connection()`: Base method with no dependencies, but prone to hang
#' with poor timeout control.
#' - `curlGetHeaders`.
#' - `RCurl::url.exists()`.
#' - urlchecker and curl packages.
#' - https://github.com/r-lib/urlchecker/blob/main/inst/tools/urltools.R
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
    ok <- isMatchingRegex(x = x, pattern = "^(ftp|http|https)://")
    if (!all(ok)) {
        return(ok)
    }
    if (is(x, "url")) {
        checkCon <- function(x) {
            test <- try(
                expr = {
                    suppressWarnings({
                        open.connection(con = x, open = "rt", timeout = 1L)
                    })
                },
                silent = TRUE
            )
            ok <- !inherits(test, "try-error")
            ok
        }
        ok <- checkCon(x)
    } else {
        checkFtp <- function(x) {
            h <- try(
                expr = {
                    curlGetHeaders(
                        url = x,
                        redirect = TRUE,
                        verify = TRUE,
                        timeout = 2L
                    )
                },
                silent = TRUE
            )
            if (inherits(h, "try-error")) {
                return(FALSE)
            }
            TRUE
        }
        checkHttp <- function(x) {
            h <- try(
                expr = {
                    curlGetHeaders(
                        url = x,
                        redirect = TRUE,
                        verify = TRUE,
                        timeout = 1L
                    )
                },
                silent = TRUE
            )
            if (inherits(h, "try-error")) {
                return(FALSE)
            }
            TRUE
        }
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
