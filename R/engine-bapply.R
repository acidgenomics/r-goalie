#' Boolean apply
#'
#' [bapply()] is a wrapper for [`vapply()`][base::vapply] that enforces
#' `FUN.VALUE = logical(1)`.
#'
#' @note
#' - `FUN.VALUE` is always set to `logical(1)`.
#' - `USE.NAMES` is always set to `TRUE`.
#'
#' @export
#' @note Updated 2019-10-22.
#'
#' @param X `atomic` or `list`.
#' @param FUN `function`.
#'   An assert check function that returns `logical(1)` boolean flag
#'   (`TRUE`/`FALSE`) to [apply][base::apply].
#' @param ... Additional arguments passed to [vapply()][base::vapply].
#'
#' @seealso
#' - `assertive.base::bapply()`.
#' - `vapply()`.
#'
#' @return `logical`.
#'
#' @examples
#' bapply(list(a = "example", b = 1), is.character)
bapply <- function(X, FUN, ...) {  # nolint
    vapply(
        X = X,
        FUN = FUN,
        FUN.VALUE = logical(1L),
        ...,
        USE.NAMES = TRUE
    )
}
