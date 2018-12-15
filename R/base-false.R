# @seealso assertive.base::false
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
