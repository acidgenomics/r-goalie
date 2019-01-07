#' Return `FALSE` scalar with cause of failure
#'
#' Always returns the value `FALSE`, with a [cause]
#' [attribute][base::attributes].
#'
#' @export
#' @inheritParams params
#'
#' @param ... Passed to [`gettextf()`][base::gettextf] to create a [cause] of
#'   failure message.
#'
#' @return `goalie`/`logical(1L)`.
#'
#' @seealso `assertive.base::false()`.
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



.causeString <- function(x) {
    stopifnot(is(x, "goalie"))
    out <- capture.output(print(x))
    # Remove the first 2 lines.
    out <- out[3L:length(out)]
    paste0(out, collapse = "\n")
}



#' @rdname false
#' @export
falseFromVector <- function(x) {
    false(.causeString(x))
}
