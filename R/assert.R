#' Assert That Certain Conditions Are True
#'
#' `assert()` is a drop-in replacement for `stopifnot()`, but is designed to
#' return more informative error messages.
#'
#' Currently, `assert()` reexports `checkmate::assert()` with a stricter default
#' setting of the `combine` argument set to `"and"` instead of `"or"`. This
#' enables `assert()` to act as a substitute for `stopifnot()`.
#'
#' @importFrom checkmate assert
#' @inheritParams checkmate::assert
#' @export
#'
#' @seealso
#' - `checkmate::assert()`.
#' - `stopifnot()`.
#' - `assertthat::assert_that()`.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assert <- checkmate::assert
# Stricter default.
formals(assert)[["combine"]] <- "and"
