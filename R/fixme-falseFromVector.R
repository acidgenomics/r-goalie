#' @rdname false
#' @export
## Updated 2019-07-29.
falseFromVector <- function(x) {
    stopifnot(is(x, "goalie"))
    false(.causeString(x))
}
