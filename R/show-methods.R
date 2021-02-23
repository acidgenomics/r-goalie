#' @name show
#' @author Michael Steinbaugh
#' @inherit AcidGenerics::show
#' @note Updated 2021-02-23.
#'
#' @examples
#' ## Match the default logical print method.
#' x <- goalie(
#'     x = c("aaa" = FALSE, "bbb" = TRUE),
#'     cause = "'aaa' is FALSE."
#' )
#' show(x)
NULL



## Updated 2021-02-23.
`show,goalie` <- function(object) {
    x <- as.logical(object)
    names(x) <- names(object)
    show(x)
}



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("goalie"),
    definition = `show,goalie`
)



## FIXME RETHINK THIS.
.print.goalie <-
    function(x, n = 10L, ignoreNA = FALSE, ...) {
    ## THIS GETS CALLED IN ASSERT ENGINE AND NEEDS TO BE RETHOUGHT...
    stop("FIXME")
    stopifnot(is.logical(x), .hasCause(x))
    lgl <- unclass(x)
    attributes(lgl) <- NULL
    print(head(lgl, n = n))
    cause <- cause(x)
    names <- names(x)
    if (is.null(names) && identical(length(x), 1L)) {
        return(cat("Cause:", cause(x)))
    }
    if (is.null(names)) {
        names <- character(length(x))  # nocov
    }
    attributes(x) <- NULL
    ok <- if (isTRUE(ignoreNA)) {
        x | is.na(x)
    } else {
        x & !is.na(x)
    }
    index <- which(!ok)
    nfail <- length(index)
    index <- head(index, n = n)
    failures <- data.frame(
        pos = index,
        value = shorten(names[index]),
        cause = unclass(cause[index]),
        row.names = seq_along(index)
    )
    header <- if (nfail > n) {
        gettextf(" (showing the first %d)", nrow(failures))
    } else {
        ""
    }
    cat(sprintf(
        fmt = "Cause: %d %s%s\n",
        nfail,
        ngettext(
            n = nfail,
            msg1 = "failure",
            msg2 = "failures"
        ),
        header
    ))
    print(failures)
}
