#' Call a function and give the result names
#'
#' Calls a function and names the result with the first argument.
#'
#' @export
#'
#' @param fun `function`.
#' @param x The first input to `fun`.
#' @param ... Additional arguments passed to `fun`.
#'
#' @seealso `assertive.base::call_and_name()`.
callAndName <- function(fun, x, ...) {
    y <- fun(x, ...)
    dim(y) <- dim(x)
    names(y) <- .toNames(x)
    y
}
