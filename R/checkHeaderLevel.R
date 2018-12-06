#' Does the Argument Contain a Markdown Header Level?
#'
#' Markdown supports header levels `1`-`7` (`<H1>`-`<H7>`).
#'
#' @name checkHeaderLevel
#' @aliases headerLevel
#' @inherit params
#'
#' @examples
#' checkHeaderLevel(1)
#' checkHeaderLevel(1L)
NULL



.headerLevel <- function(x) {
    if (!is_scalar_integerish(x)) {
        return(FALSE)
    }
    x %in% seq_len(7L)
}



#' @rdname checkHeaderLevel
#' @export
checkHeaderLevel <- function(x) {
    if (isTRUE(.headerLevel(x))) {
        TRUE
    } else {
        "Markdown supports header levels 1-7"
    }
}



#' @rdname checkHeaderLevel
#' @export
check_header_level <- checkHeaderLevel



#' @rdname checkHeaderLevel
#' @export
testHeaderLevel <- makeTestFunction(checkHeaderLevel)



#' @rdname checkHeaderLevel
#' @export
test_header_level <- testHeaderLevel



#' @rdname checkHeaderLevel
#' @export
assertHeaderLevel <- makeAssertionFunction(checkHeaderLevel)



#' @rdname checkHeaderLevel
#' @export
assert_header_level <- assertHeaderLevel



#' @rdname checkHeaderLevel
#' @export
expect_header_level <- makeExpectationFunction(checkHeaderLevel)
