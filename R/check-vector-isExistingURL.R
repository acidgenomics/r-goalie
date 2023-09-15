## FIXME This needs to timeout on failure faster still...argh.
## isAnExistingURL("ftp://ftp.ensembl.org/pub/")



#' Does the input contain an existing (active) URL?
#'
#' @name check-vector-isExistingURL
#' @note Updated 2023-09-15.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `open.connection()`.
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

## FIXME Names of isURL check isn't what we want.

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
    assert(requireNamespaces("RCurl"))
    ok <- RCurl::url.exists(x)
    names(ok) <- .xname
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
