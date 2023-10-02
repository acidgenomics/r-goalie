#' Does the input contain an existing (active) URL?
#'
#' @name check-vector-isExistingUrl
#' @note Updated 2023-10-02.
#'
#' @details
#' Supports HTTPS, HTTP, and FTP protocols.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `curlGetHeaders()`: Amazing function with good timeout control.
#' - `open.connection()`: Base method with no dependencies, but prone to hang
#' with poor timeout control.
#' - `RCurl::url.exists()`: Seems to be good but adds external dependency.
#' - urlchecker and curl packages.
#' - https://github.com/r-lib/urlchecker/blob/main/inst/tools/urltools.R
#' - https://stackoverflow.com/questions/52911812
#' - https://stackoverflow.com/a/17620732/3911732
#'
#' @examples
#' ## TRUE ====
#' isAnExistingUrl("https://acidgenomics.com/")
#'
#' ## FALSE ====
#' isAnExistingUrl("https://failwhale.acidgenomics.com/")
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
#' @section FTP server status codes:
#'
#' - 1xx: positive preliminary reply
#' - 2xx: positive completion reply
#' - 3xx: positive intermediate reply
#' - 4xx: transient negative completion reply
#' - 5xx: permanent negative completion reply
#' - 6xx: protected reply
#'
#' Only codes >= 400 represent errors.
#'
#' @param x `character(1)`.
#'
#' @return `logical(1)`.
#'
#' @seealso
#' - https://en.wikipedia.org/wiki/List_of_FTP_server_return_codes
#' - https://www.rfc-editor.org/rfc/rfc959
#' Section 4.2.2 "Numeric Order List of Reply Codes"
#' - https://www.rfc-editor.org/rfc/rfc2228
#' Section 5 "New FTP Replies".
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
                timeout = 3L
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



#' Check an HTTP(S) URL
#'
#' @note Updated 2023-09-22.
#' @noRd
#'
#' @section HTTP server status codes:
#'
#' - 1xx: informational response
#' - 2xx: successful
#' - 3xx: redirection
#' - 4xx: client error
#' - 5xx: server error
#'
#' Only codes >= 400 represent errors.
#'
#' @param x `character(1)`.
#'
#' @return `logical(1)`.
#'
#' @seealso
#' - https://developer.mozilla.org/en-US/docs/Web/HTTP/Status
#' - https://en.wikipedia.org/wiki/List_of_HTTP_status_codes
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
    ok <- grepl(pattern = "302", x = h[[1L]])
    if (isTRUE(ok)) {
        return(TRUE)
    }
    status <- attr(h, "status")
    ok <- status < 400L
    if (!isTRUE(ok)) {
        return(FALSE)
    }
    ok
}



## Vector ======================================================================

#' @describeIn check-vector-isExistingUrl Vectorized.
#' @export
isExistingUrl <- function(x) {
    if (is(x, "url")) {
        return(ifelse(
            test = .checkCon(x),
            yes = TRUE,
            no = false("URL doesn't exist.")
        ))
    }
    ok <- isUrl(x)
    if (!all(ok)) {
        return(ok)
    }
    ok <- bapply(
        X = x,
        FUN = function(x) {
            switch(
                EXPR = strsplit(x, split = ":")[[1L]][[1L]],
                "ftp" = .checkFtp(x),
                "http" = .checkHttp(x),
                "https" = .checkHttp(x)
            )
        }
    )
    names(ok) <- toCauseNames(x)
    setCause(ok, false = "URL doesn't exist")
}



## Scalar ======================================================================

#' @describeIn check-vector-isExistingUrl Scalar. Requires a single URL.
#' @export
isAnExistingUrl <- function(x) {
    ok <- isScalar(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isExistingUrl(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}



#' @describeIn check-vector-isExistingUrl Scalar. Checks that all strings are
#' existing URLs.
#' @export
allAreExistingUrls <- function(x) {
    ok <- isExistingUrl(x)
    if (!all(ok)) {
        return(falseFromVector(ok))
    }
    TRUE
}
