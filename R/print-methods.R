#' Print methods for objects with a cause attribute
#'
#' @name print
#' @note Updated 2019-10-18.
#'
#' @inheritParams acidroxygen::params
#' @param ignoreNA `logical(1)`.
#'   Should NA values pass (`TRUE`) or fail (`FALSE`)?
#'
#'   - `TRUE`: OK can be `TRUE` or `NA`; `FALSE` is bad.
#'   - `FALSE`: OK can be `TRUE`; `FALSE` or `NA` is bad.
#'
#' @return Print command and return invisibly.
NULL



#' @rdname print
#' @method print goalie
#' @export
print.goalie <- function(x, n = 10L, ignoreNA = FALSE, ...) {
    stopifnot(is.logical(x), .hasCause(x))
    lgl <- unclass(x)
    attributes(lgl) <- NULL
    print(head(lgl, n = n))
    cause <- cause(x)
    names <- names(x)
    if (is.null(names) && identical(length(x), 1L)) {
        return(cat("cause:", cause(x), sep = " "))
    }
    if (is.null(names)) {
        names <- character(length(x))
    }
    x <- .stripAttributes(x)
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
        value = .truncate(names[index]),
        cause = unclass(cause[index]),
        row.names = seq_along(index)
    )
    header <- if (nfail > n) {
        gettextf(" (showing the first %d)", nrow(failures))
    } else {
        ""
    }
    cat(sprintf(
        fmt = ngettext(
            n = nfail,
            msg1 = "cause: %d failure%s\n",
            msg2 = "cause: %d failures%s\n"
        ),
        nfail, header
    ))
    print(failures)
}
