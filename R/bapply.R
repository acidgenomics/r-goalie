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
#' @note Updated 2021-02-23.
#'
#' @param X `atomic` or `list`.
#'
#' @param FUN `function`.
#' An assert check function that returns `logical(1)` boolean flag
#' (`TRUE`/`FALSE`) to [apply][base::apply].
#'
#' @param ... Additional arguments passed to [vapply()][base::vapply].
#'
#' @param USE.NAMES `logical(1)`.
#' If `TRUE` and `X` is character, use `X` as [`names`][base::names] for the
#' result, unless it has names already.
#'
#' @seealso
#' - `assertive.base::bapply()`.
#' - `vapply()`.
#'
#' @return `logical`.
#'
#' @examples
#' bapply(X = list(a = "example", b = 1), FUN = is.character)
bapply <- function(X, FUN, ..., USE.NAMES = TRUE) { # nolint
    vapply(
        X = X,
        FUN = FUN,
        FUN.VALUE = logical(1L),
        ...,
        USE.NAMES = USE.NAMES
    )
}
