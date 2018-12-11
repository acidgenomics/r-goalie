# TODO Improve assert engine messages, following assertthat approach.



#' Assert That Certain Conditions Are True
#'
#' `assert()` is a drop-in replacement for `stopifnot()`, but is designed to
#' return more informative error messages.
#'
#' Currently, `assert()` reexports `assertthat::assert_that()`.
#'
#' @importFrom assertthat assert_that
#' @inheritParams assertthat::assert_that
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
assert <- assert_that
