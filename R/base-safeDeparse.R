#' Expression deparsing
#'
#' Turn unevaluated expressions into character strings.
#'
#' [safeDeparse()] is modified version of [`deparse()`][base::deparse] that
#' always returns `character(1)`.
#'
#' @export
#' @note Updated 2019-07-29.
#'
#' @param expr `expression`.
#'   Any R expression.
#' @param ... Passed to [`deparse()`][base::deparse].
#'
#' @seealso
#' - `assertive.base::safe_deparse()`.
#' - `deparse()`.
#'
#' @return `character(1)`.
#'
#' @examples
#' safeDeparse(is.character("a"))
safeDeparse <- function(expr, ...) {
    paste0(deparse(expr, width.cutoff = 500L, ...), collapse = "")
}
