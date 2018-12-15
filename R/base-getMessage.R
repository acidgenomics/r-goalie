# Consider using `safeDeparse()` here.
# @seealso `assertthat:::fail_default`.
failDefault <- function(call, env) {
    out <- deparse(call, width.cutoff = 60L)
    if (length(out) > 1L) {
        out <- paste0(out[1L], "...")
    }
    paste0(out, " is not TRUE")
}



# https://github.com/hadley/assertthat/blob/master/R/base.r
baseFS <- new.env(parent = emptyenv())



# @seealso `assertthat:::get_message`.
getMessage <- function(res, call, env = parent.frame()) {
    stopifnot(is.call(call), length(call) >= 1)
    if (hasAttr(res, "msg")) {
        return(attr(res, "msg"))
    }
    f <- eval(call[[1]], env)
    if (!is.primitive(f))
        call <- match.call(f, call)
    fname <- deparse(call[[1]])
    fail <- onFailure(f) %||% baseFS[[fname]] %||% failDefault
    fail(call, env)
}
