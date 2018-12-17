#' Apply a Function over a List or Vector
#'
#' `bapply` is a wrapper to `vapply` for functions that return `logical(1)`.
#'
#' @note `USE.NAMES` is always set to TRUE.
#'
#' @param x `atomic` or `list`.
#' @param predicate `function`. A predicate function that returns `logical(1)`
#'   (boolean flag) to `apply`.
#' @param ... Additional arguments passed to [vapply].
#'
#' @seealso
#' - `assertive.base::bapply`.
#' - `vapply`.
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
