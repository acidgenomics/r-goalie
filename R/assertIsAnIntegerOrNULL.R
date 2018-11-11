#' Assert Is an Integer or NULL
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertIsAnIntegerOrNULL(1L)
#' assertIsAnIntegerOrNULL(NULL)
assertIsAnIntegerOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("integer", "NULL"))
    if (is.integer(object)) {
        assert_is_an_integer(object)
    }
}
