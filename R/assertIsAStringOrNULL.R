#' Assert Is a String or NULL
#'
#' @inherit assert
#' @export
#'
#' @examples
#' assertIsAStringOrNULL("hello world")
#' assertIsAStringOrNULL(NULL)
assertIsAStringOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("character", "NULL"))
    if (is.character(object)) {
        assert_is_a_string(object)
    }
}
