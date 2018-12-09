#' Are These Valid Names?
#'
#' @inherit params
#' @export
#'
#' @seealso
#' - `make.names()`.
#' - `basejump::makeNames()`.
#'
#' @examples
#' ## Dots (periods) and underscores are valid.
#' checkValidNames(c("sample.1", "sample_1"))
#'
#' ## Can't begin with a number.
#' testValidNames("293cells")
#'
#' ## Spaces, dashes (hyphens), and other non-alphanumerics aren't valid.
#' testValidNames("sample 1")
#' testValidNames("cell-AAAAAAAA")
#' testValidNames("GFP+")
checkValidNames <- function(names) {
    if (
        !is.character(names) ||
        length(names) == 0L
    ) {
        return("Must contain non-empty character")
    }

    ok <- identical(names, make.names(names, unique = TRUE))
    if (!ok) {
        return(paste(
            "Not all names are valid in R.",
            "See make.names() documentation for details on valid names."
        ))
    }

    TRUE
}



#' @rdname checkValidNames
#' @export
check_valid_names <- checkValidNames  # nolint



#' @rdname checkValidNames
#' @export
testValidNames <- makeTestFunction(checkValidNames)



#' @rdname checkValidNames
#' @export
test_valid_names <- testValidNames  # nolint



#' @rdname checkValidNames
#' @export
assertValidNames <- makeAssertionFunction(checkValidNames)



#' @rdname checkValidNames
#' @export
assert_valid_names <- assertValidNames  # nolint



#' @rdname checkValidNames
#' @export
expect_valid_names <- makeExpectationFunction(checkValidNames)  # nolint
