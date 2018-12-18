# @seealso `assertthat::see_if()`.
seeIf <- function(..., env = parent.frame(), msg = NULL) {
    asserts <- eval(substitute(alist(...)))
    for (assertion in asserts) {
        res <- tryCatch(
            expr = {
                eval(assertion, env)
            },
            assertError = function(e) {
                structure(FALSE, msg = e$message)
            }
        )
        checkResult(res)
        if (!isTRUE(res)) {
            if (is.null(msg)) {
                msg <- getMessage(res, assertion, env)
            }
            return(structure(FALSE, msg = msg))
        }
    }
    res
}
