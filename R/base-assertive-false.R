#' Return `FALSE` with a cause of failure
#'
#' Always returns the value `FALSE`, with a `cause`
#' [attribute][base::attributes].
#'
#' @export
#'
#' @param ... Passed to [gettextf()][base::gettextf] to create a [cause] of
#'   failure message.
#'
#' @seealso `assertive.base::false()`.
false <- function(...) {
    msg <- if (nargs() > 0L) {
        sprintf(...)
    } else {
        ""
    }
    x <- FALSE
    cause(x) <- msg[1L]
    class(x) <- c("scalar_with_cause", "logical")
    x
}
