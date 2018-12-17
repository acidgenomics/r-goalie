# @seealso `assertthat::on_failure`.
# Note that assertive uses cause instead of fail.
onFailure <- function(x) {
    attr(x, "fail")
}



# @seealso `assertthat::on_failure`.
# Note that assertive uses cause instead of fail.
`onFailure<-` <- function(x, value) {
    stopifnot(
        is.function(x),
        identical(names(formals(value)), c("call", "env"))
    )
    attr(x, "fail") <- value
    x
}
