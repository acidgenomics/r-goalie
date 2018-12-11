# NOTE Don't use `assert_that()` as a general `stopifnot()` replacement.
# It doesn't catch everything, and isn't hardened for S4 methods.

#' Assert That Certain Conditions Are True
#'
#' Currently, we're reexporting `stopifnot()` as `assert()` verbatim here.
#'
#' @inheritParams base::stopifnot
#' @export
#'
#' @param ... Any number of expressions that return `logical(1)`, each of which
#'   should evaluate to `TRUE` on success and `FALSE` on failure.
#'
#' @seealso
#' - `stopifnot()`.
#' - `checkmate::assert()`.
#' - `assertthat::assert_that()`.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assert <- stopifnot
