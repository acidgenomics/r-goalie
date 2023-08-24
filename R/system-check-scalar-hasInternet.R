## nocov start

## FIXME We need to make this fail more gracefully on a connection error...
## set a timeout here.
## goalie::hasInternet(url = "https://rest.ensembl.org/")
##
## FIXME Use `isAnExistingURL
## FIXME Create these:
## `isExistingURL`
## `isAnExistingURL`
## `allAreExistingURLs`
##
## FIXME Then wrap `hasInternet` with t hat

## FIXME Take out the URL parameter here, can be abused. Rework as an assert
## check instead.


#' Does the current session have an internet connection?
#'
#' @name check-scalar-hasInternet
#' @note Updated 2023-08-24.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `open.connection()`.
#' - `Biobase::testBioCConnection()`.
#' - `curl::has_internet()`.
#' - `curl::nslookup()`.
#' - `RCurl::getURL()`.
#' - https://stackoverflow.com/questions/52911812
#' - https://stackoverflow.com/a/17620732/3911732
#'
#' @examples
#' hasInternet()
NULL


## FIXME Rename this to isAValidURL...

## FIXME Rework the fail messages here.
## FIXME Always need to try to close connection here.

## FIXME Rework this to use `isAnActiveURL check internally.

#' @rdname check-scalar-hasInternet
#' @export
hasInternet <- function(url = "http://www.bioconductor.org/") {
    ok <- as.logical(capabilities(what = "http/ftp"))
    if (!isTRUE(ok)) {
        return(false("R session does not have Internet access."))
    }
    con <- url(url)
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
    if (!isTRUE(ok)) {
        return(false("URL connection failure: {.url %s}.", url))
    }
    TRUE
}



## nocov end
