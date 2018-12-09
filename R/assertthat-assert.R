#' Assert That Certain Conditions Are True
#'
#' `assert()` is a drop-in replacement for `stopifnot()`, but is designed to
#' return more informative error messages.
#'
#' Currently, `assert()` reexports `assertthat::assert_that()`.
#'
#' @importFrom checkmate assert
#' @inheritParams checkmate::assert
#' @export
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
assert <- assertthat::assert_that
