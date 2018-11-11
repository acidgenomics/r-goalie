#' Has Rownames?
#'
#' A stricter alternative to the assertive version that works properly with
#' data frames.
#'
#' @inherit params
#' @export
#'
#' @note Also, `tibble::has_rownames()` appears to be more consistent than
#' `assertive.properties::has_rownames()` for `DataFrame` and `tbl_df` class.
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
        # Check for rowname column for classes that inherit data.frame but
        # don't allow rownames to be set.
        "rowname" %in% colnames(x)
    } else if (identical(
        x = as.character(rownames(x)),
        y = as.character(seq_len(nrow(x)))
    )) {
        # Check for numeric rownames that match rows.
        FALSE
    } else {
        has_rownames(x)
    }
}

#' @rdname hasRownames
#' @export
assertHasRownames <- function(x) {
    assert_that(hasRownames(x))
    if (!is(x, "tbl_df")) {
        assert_are_disjoint_sets(
            x = rownames(x),
            y = as.character(seq_len(nrow(x)))
        )
    }
}
