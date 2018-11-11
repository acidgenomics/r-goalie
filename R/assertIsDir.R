#' Assert Is Directory
#'
#' @inherit params
#' @inheritParams params
#' @export
#'
#' @examples
#' assertIsDir("~")
assertIsDir <- function(dir) {
    assert_is_a_string(dir)
    assert_that(dir.exists(dir))
}
