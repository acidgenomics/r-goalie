#' Boolean apply
#'
#' [bapply()] is a wrapper for [vapply()][base::vapply] that enforces
#' `FUN.VALUE = logical(1)`.
#'
#' @note `USE.NAMES` is always set to TRUE.
#'
#' @param x `atomic` or `list`.
#' @param predicate `function`.
#'   A predicate function that returns `logical(1)` boolean flag
#'   (`TRUE`/`FALSE`) to [apply][base::apply].
#' @param ... Additional arguments passed to [vapply()][base::vapply].
#'
#' @seealso
#' - `assertive.base::bapply()`.
#' - `vapply()`.
#'
#' @return `logical`.
#' @export
#'
#' @examples
#' bapply(list(a = "example", b = 1), is.character)
bapply <- function(x, predicate, ...) {
    vapply(
        X = x,
        FUN = predicate,
        FUN.VALUE = logical(1L),
        ...,
        USE.NAMES = TRUE
    )
}
