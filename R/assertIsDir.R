#' Assert Is Directory
#'
#' @inherit assert
#'
#' @export
#'
#' @examples
#' assertIsDir("~")
assertIsDir <- function(object) {
    assert_is_a_string(object)
    assert_all_are_dirs(object)
}
