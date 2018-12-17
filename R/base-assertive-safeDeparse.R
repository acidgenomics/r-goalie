#' Expression Deparsing
#'
#' Turn unevaluated expressions into character strings.
#'
#' `safeDeparse` is modified version of `deparse` that is guaranteed to always
#' return `character(1)`.
#'
#' @export
#'
#' @param expr `expression`.
#'   Any R expression.
#' @param ... Passed to `deparse`.
#'
#' @seealso
#' - `assertive.base::safe_deparse`.
#' - `deparse`.
#'
#' @examples
#' safeDeparse(is.character("a"))
safeDeparse <- function(expr, ...) {
    paste0(deparse(expr, width.cutoff = 500L, ...), collapse = "")
}
