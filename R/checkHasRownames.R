#' Does the Object Have Rownames?
#'
#' A stricter alternative to the assertive version that works properly with
#' data frames.
#'
#' @name checkHasRownames
#' @inherit params
#'
#' @examples
#' ## Pass ====
#' x <- data.frame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#' checkHasRownames(x)
#'
#' ## Fail ====
#' x <- data.frame(a = seq_len(2L))
#' print(x)
#' # Standard data frame doesn't allow NULL row names.
#' rownames(x)
#' checkHasRownames(x)
#'
#' x <- S4Vectors::DataFrame(a = seq_len(2L))
#' print(x)
#' # S4 data frame does allow NULL row names.
#' rownames(x)
#' checkHasRownames(x)
NULL



.hasRownames <- function(x) {
    if (
        is(x, "data.table") ||
        is(x, "tbl_df")
    ) {
        # Check for "rowname" column in classes that inherit data.frame but
        # don't allow row names to be set.
        "rowname" %in% colnames(x)
    } else if (
        is(x, "data.frame") &&
        identical(
            x = as(rownames(x), "character"),
            y = as(seq_len(nrow(x)), "character")
        )
    ) {
        # Standard data.frame doesn't allow NULL row names to be set, which is
        # a poor default choice. So in this case, check for a numeric sequence
        # that is identical to the number of rows.
        FALSE
    } else {
        !is.null(rownames(x))
    }
}



#' @rdname checkHasRownames
#' @export
checkHasRownames <- function(x) {
    if (isTRUE(.hasRownames(x))) {
        TRUE
    } else {
        "Object does not have row names defined"
    }
}



#' @rdname checkHasRownames
#' @export
check_has_rownames <- checkHasRownames



#' @rdname checkHasRownames
#' @export
testHasRownames <- makeTestFunction(checkHasRownames)



#' @rdname checkHasRownames
#' @export
test_has_rownames <- testHasRownames



#' @rdname checkHasRownames
#' @export
assertHasRownames <- makeAssertionFunction(checkHasRownames)



#' @rdname checkHasRownames
#' @export
assert_has_rownames <- assertHasRownames



#' @rdname checkHasRownames
#' @export
expect_has_rownames <- makeExpectationFunction(checkHasRownames)
