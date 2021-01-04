#' Return `FALSE` scalar with cause of failure
#'
#' Always returns the value `FALSE`, with a [cause]
#' [attribute][base::attributes].
#'
#' @name false
#' @note Updated 2021-01-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Passed to [`gettextf()`][base::gettextf] to create a [cause] of
#'   failure message.
#'
#' @return `goalie`/`logical(1L)`.
#'
#' @seealso `assertive.base::false()`.
NULL



#' @rdname false
#' @export
## Updated 2019-07-29.
false <- function(...) {
    msg <- if (nargs() > 0L) {
        sprintf(...)
    } else {
        ""
    }
    x <- FALSE
    cause(x) <- msg[[1L]]
    x
}



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



#' @rdname false
#' @export
## Updated 2019-07-29.
falseFromVector <- function(x) {
    false(.causeString(x))
}
