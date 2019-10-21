#' Call a function and give the result names
#'
#' Calls a function and names the result with the first argument.
#'
#' @export
#' @note Updated 2019-07-29.
#'
#' @inheritParams acidroxygen::params
#' @param x The first input to `fun`.
#' @param ... Additional arguments passed to function defined in `fun`.
#'
#' @seealso `assertive.base::call_and_name()`.
#'
#' @return The result of `fun(x, ...)`, with the names given by the argument
#'   `x`.
#'
#' @examples
#' callAndName(is.finite, c(1, Inf, NA))
callAndName <- function(fun, x, ...) {
    y <- fun(x, ...)
    dim(y) <- dim(x)
    names(y) <- toNames(x)
    y
}
