# TODO Decide between this or assertive approach.



# @seealso `assertthat::on_failure()`.
# Note that assertive sets "cause" attribute instead of "fail".
onFailure <- function(x) {
    attr(x, "fail")
}



# @seealso `assertthat::on_failure()`.
# Note that assertive sets "cause" attribute instead of "fail".
`onFailure<-` <- function(x, value) {
    stopifnot(
        is.function(x),
        identical(names(formals(value)), c("call", "env"))
    )
    attr(x, "fail") <- value
    x
}
