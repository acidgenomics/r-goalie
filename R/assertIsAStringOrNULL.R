#' Assert Is a String or NULL
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertIsAStringOrNULL("hello world")
#' assertIsAStringOrNULL(NULL)
assertIsAStringOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("character", "NULL"))
    if (is.character(x)) {
        assert_is_a_string(x)
    }
}
