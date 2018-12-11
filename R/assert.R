#' Assert That Certain Conditions Are True
#'
#' `assert()` is a drop-in replacement for `stopifnot()`, but is designed to
#' return more informative error messages.
#'
#' @inheritParams base::stopifnot
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
assert <- stopifnot
