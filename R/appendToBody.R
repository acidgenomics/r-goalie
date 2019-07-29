#' Append values to function body
#'
#' @note Updated 2019-07-15.
#' @export
#'
#' @param fun `function`.
#' @param values `call`.
#'   Quoted values (i.e. call) to slot in the function body.
#' @param after `integer(1)`.
#'   Where to append in the `body`.
#'   The default of `1` places directly after the opening curly bracket.
#'
#' @seealso [Stack Overflow](https://stackoverflow.com/questions/38732663)
#'
#' @return `function`.
#'
#' @examples
#' ## Add a deprecation call into function body.
#' x <- function() {
#'     "hello"
#' }
#' body(x)
#' x <- appendToBody(x, quote(.Deprecated("y")))
#' body(x)
appendToBody <- function(fun, values, after = 1L) {
    stopifnot(
        is.function(fun),
        is.call(values),
        is.integer(after)
    )
    b <- body(fun)
    b <- as.list(b)
    ## Hardening against 1 liners and/or lack of curly brackets.
    stopifnot(identical(b[[1L]], as.name("{")))
    b <- append(b, values = values, after = after)
    b <- as.call(b)
    body(fun) <- b
    fun
}
