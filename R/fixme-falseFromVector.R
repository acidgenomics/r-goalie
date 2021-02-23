## FIXME NEED TO RETHINK THIS APPROACH, NOT USING PRINT METHOD...



## Note that this will intentionally fail if you pass in a logical vector
## without a goalie cause attribute.
## Updated 2019-07-29.
.causeString <- function(x) {
    stopifnot(is(x, "goalie"))
    out <- capture.output(print(x))
    ## Remove the first 2 lines.
    out <- out[3L:length(out)]
    paste0(out, collapse = "\n")
}



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
            value = .shorten(names[index]),
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



#' @rdname false
#' @export
## Updated 2019-07-29.
falseFromVector <- function(x) {
    false(.causeString(x))
}
