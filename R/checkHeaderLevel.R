#' Does the Argument Contain a Markdown Header Level?
#'
#' Markdown supports header levels `1`-`7` (`<H1>`-`<H7>`).
#'
#' @name checkHeaderLevel
#' @aliases headerLevel header_level
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' checkHeaderLevel(1)
#'
#' ## Fail ====
#' checkHeaderLevel(0)
checkHeaderLevel <- function(x) {
    ok <- is_scalar_integerish(x)
    if (!ok) {
        return("Must be scalar integerish")
    }

    ok <- x %in% seq_len(7L)
    if (!ok) {
        return("Markdown supports header levels 1-7")
    }

    TRUE
}



#' @describeIn checkHeaderLevel snake alias.
#' @export
check_header_level <-  # nolint
    checkHeaderLevel



#' @rdname checkHeaderLevel
#' @export
testHeaderLevel <- makeTestFunction(checkHeaderLevel)



#' @describeIn checkHeaderLevel snake alias.
#' @export
test_header_level <-  # nolint
    testHeaderLevel



#' @rdname checkHeaderLevel
#' @export
assertHeaderLevel <- makeAssertionFunction(checkHeaderLevel)



#' @describeIn checkHeaderLevel snake alias.
#' @export
assert_header_level <-  # nolint
    assertHeaderLevel



#' @rdname checkHeaderLevel
#' @export
expect_header_level <-  # nolint
    makeExpectationFunction(checkHeaderLevel)
