#' Assert Is an Integer or NULL
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertIsAnIntegerOrNULL(1L)
#' assertIsAnIntegerOrNULL(NULL)
assertIsAnIntegerOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("integer", "NULL"))
    if (is.integer(x)) {
        assert_is_an_integer(x)
    }
}
