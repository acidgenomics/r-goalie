#' Assert Is a Number or NULL
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertIsANumberOrNULL(1.1)
#' assertIsANumberOrNULL(NULL)
assertIsANumberOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("numeric", "NULL"))
    if (is.numeric(x)) {
        assert_is_a_number(x)
    }
}
