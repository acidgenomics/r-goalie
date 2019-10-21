#' Append values to function body
#'
#' @export
#' @note Updated 2019-08-23.
#'
#' @inheritParams acidroxygen::params
#' @param values `call`.
#'   Quoted values (i.e. call) to slot in the function body.
#' @param after `integer(1)`.
#'   Where to append in the `body`.
#'   The default of `1L` places directly after the opening curly bracket.
#'
#' @seealso [Stack Overflow](https://stackoverflow.com/questions/38732663)
#'
#' @return `function`.
#'
#' @examples
#' ## Add a deprecation call into function body.
#' fun <- function() { print("hello world") }
#' body(fun)
#'
#' ## values: call ====
#' values <- as.call(quote(.Deprecated("XXX")))
#' x <- appendToBody(fun = fun, values = values)
#' body(x)
#'
#' ## values: list ====
#' values <- list(
#'     quote(print("AAA")),
#'     quote(print("BBB"))
#' )
#' x <- appendToBody(fun = fun, values = values)
#' body(x)
appendToBody <- function(fun, values, after = 1L) {
    stopifnot(
        is.function(fun),
        is.call(values) || is.list(values),
        is.integer(after)
    )
    if (is.list(values)) {
        stopifnot(all(vapply(
            X = values,
            FUN = is.call,
            FUN.VALUE = logical(1L)
        )))
    }
    b <- body(fun)
    b <- as.list(b)
    ## Hardening against 1 liners and/or lack of curly brackets.
    stopifnot(identical(b[[1L]], as.name("{")))
    b <- append(b, values = values, after = after)
    b <- as.call(b)
    body(fun) <- b
    fun
}
