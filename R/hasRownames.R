#' Does the Object Have Rownames?
#'
#' A stricter alternative to the assertive version that works properly with
#' data frames.
#'
#' @inherit params
#' @export
#'
#' @examples
#' x <- S4Vectors::DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#' hasRownames(x)
hasRownames <- function(x) {
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

#' @rdname hasRownames
#' @export
has_rownames <- hasRownames



#' @rdname hasRownames
#' @export
assertHasRownames <- makeAssertionFunction(hasRownames)

#' @rdname hasRownames
#' @export
assert_has_rownames <- assertHasRownames
