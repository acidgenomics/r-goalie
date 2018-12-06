#' Assert That Certain Conditions Are True
#'
#' `assert()` behaves like `stopifnot()` but supports more informative error
#' messages. The function supports multiple individual assertions must return a
#' `boolean flag` (i.e. a single `TRUE` or `FALSE` `logical`).
#'
#' @importFrom assertthat assert_that
#' @inheritParams assertthat::assert_that
#' @inheritParams params
#' @export
#'
#' @seealso
#' - `stopifnot()`.
#' - `assertthat::assert_that()`.
#'
#' @examples
#' assert(
#'     is.atomic("example"),
#'     is.character("example")
#' )
assertthat::assert_that -> assert
