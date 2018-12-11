#' Assert That Certain Conditions Are True
#'
#' `assert()` is a drop-in replacement for `stopifnot()`, but is designed to
#' return more informative error messages.
#'
#' @inheritParams base::stopifnot
#' @export
#'
#' @param ... Any number of expressions that return `logical(1)`, each of which
#'   should evaluate to `TRUE` on success and `FALSE` on failure.
#'
#' @seealso
#' - `stopifnot()`.
#' - `assertthat::assert_that()`.
#' - `checkmate::assert()`.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assert <- stopifnot
