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



## Internal functions ==========================================================

#' Check a URL connection
#'
#' @note Updated 2023-09-15.
#' @noRd
#'
#' @param x `url`.
#'
#' @return `logical(1)`.
#'
#' @examples
#' ## TRUE
#' con <- url("https://google.com/")
#' .checkCon(con)
#' close(con)
#' ## FALSE
#' con <- url("http://ftp.wormbase.org/")
#' .checkCon(con)
#' close(con)
.checkCon <- function(x) {
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



#' Check an FTP URL
#'
#' @note Updated 2023-09-15.
#' @noRd
#'
#' @section FTP server error codes:
#' Only need those >= 400.
#'
#' @param x `character(1)`.
#'
#' @return `logical(1)`.
#'
#' @seealso
#' - https://en.wikipedia.org/wiki/List_of_FTP_server_return_codes
#' - https://www.rfc-editor.org/rfc/rfc959
#'   Section 4.2.2 "Numeric Order List of Reply Codes"
#' - https://www.rfc-editor.org/rfc/rfc2228
#'   Section 5 "New FTP Replies".
#' - https://github.com/r-lib/urlchecker/blob/main/inst/tools/urltools.R
#'
#' @examples
#' ## TRUE
#' .checkFtp("ftp://ftp.ensembl.org/pub/release-110/mysql/")
#' .checkFtp("ftp://ftp.wormbase.org/")
#'
#' ## FALSE
#' .checkFtp("ftp://failwhale.acidgenomics.com/")
#' .checkFtp("ftp://download.nvidia.com/")
.checkFtp <- function(x) {
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
    ok <- !inherits(h, "try-error")
    if (!isTRUE(ok)) {
        return(FALSE)
    }
    status <- attr(h, "status")
    ok <- status < 400L
    if (!isTRUE(ok)) {
        return(FALSE)
    }
    TRUE
}



#' Check an HTTP or HTTPS URL
#'
#' @note Updated 2023-09-15.
#' @noRd
#'
#' @section HTTP server error codes:
#' Only need those >= 400.
#'
#' @param x `character(1)`.
#'
#' @return `logical(1)`.
#'
#' @seealso
#' - https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
#' - https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml
#'
#' @examples
#' ## TRUE
#' .checkHttp("https://bioconductor.org/")
#' .checkHttp("https://google.com/")
#'
#' ## FALSE
#' .checkHttp("https://failwhale.acidgenomics.com/")
#' .checkHttp("https://acidgenomics.com/404")
.checkHttp <- function(x) {
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
    ok <- !inherits(h, "try-error")
    if (!isTRUE(ok)) {
        return(FALSE)
    }
    status <- attr(h, "status")
    ok <- status < 400L
    if (!isTRUE(ok)) {
        return(FALSE)
    }
    ok
}



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
        ok <- .checkCon(x)
    } else {
        protocol <- strsplit(x, split = ":")[[1L]][[1L]]
        ok <- switch(
            EXPR = protocol,
            "ftp" = .checkFtp(x),
            "http" = .checkHttp(x),
            "https" = .checkHttp(x)
        )
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
