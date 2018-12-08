#' Does the Object Have Column Names?
#'
#' @name checkHasColnames
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' x <- data.frame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#' checkHasColnames(x)
#'
#' ## Fail ====
#' x <- data.frame()
#' print(x)
#' colnames(x)
#' checkHasColnames(x)
#'
#' x <- S4Vectors::DataFrame()
#' print(x)
#' colnames(x)
#' checkHasColnames(x)
checkHasColnames <- function(x) {
    ok <- length(colnames(x)) > 0L
    if (!ok) {
        return("Object does not have column names")
    }
    TRUE
}



#' @rdname checkHasColnames
#' @export
check_has_colnames <- checkHasColnames



#' @rdname checkHasColnames
#' @export
testHasColnames <- makeTestFunction(checkHasColnames)



#' @rdname checkHasColnames
#' @export
test_has_colnames <- testHasColnames



#' @rdname checkHasColnames
#' @export
assertHasColnames <- makeAssertionFunction(checkHasColnames)



#' @rdname checkHasColnames
#' @export
assert_has_colnames <- assertHasColnames



#' @rdname checkHasColnames
#' @export
expect_has_colnames <- makeExpectationFunction(checkHasColnames)
