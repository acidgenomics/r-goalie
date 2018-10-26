#' Assert Is a Number or NULL
#'
#' @inherit assert
#' @export
#'
#' @examples
#' assertIsANumberOrNULL(1.1)
#' assertIsANumberOrNULL(NULL)
assertIsANumberOrNULL <- function(object) {
    assert_is_any_of(object, classes = c("numeric", "NULL"))
    if (is.numeric(object)) {
        assert_is_a_number(object)
    }
}
