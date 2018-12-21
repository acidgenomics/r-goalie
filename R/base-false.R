#' Return `FALSE` scalar with cause of failure
#'
#' Always returns the value `FALSE`, with a `cause`
#' [attribute][base::attributes].
#'
#' @export
#'
#' @param ... Passed to [gettextf()][base::gettextf] to create a [cause] of
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
    paste0(capture.output(print(x))[-1L], collapse = "\n")
}



#' @rdname false
#' @export
falseFromVector <- function(x) {
    false(.causeString(x))
}
