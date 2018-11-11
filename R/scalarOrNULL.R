#' Is an Integer (or NULL)?
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



#' Is a Number (or NULL)?
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



#' Is String (or NULL)?
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertIsAStringOrNULL("hello world")
#' assertIsAStringOrNULL(NULL)
assertIsStringOrNULL <- function(x) {
    assert_is_any_of(x, classes = c("character", "NULL"))
    if (is.character(x)) {
        assert_is_a_string(x)
    }
}
