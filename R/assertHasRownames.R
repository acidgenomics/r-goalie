#' Assert Has Rownames
#'
#' A stricter alternative to the assertive version that works properly with
#' data frames.
#'
#' @inherit assert
#' @export
#'
#' @examples
#' object <- DataFrame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(object)
#' assertHasRownames(object)
assertHasRownames <- function(object) {
    assert_all_are_true(hasRownames(object))
    if (!is(object, "tbl_df")) {
        assert_are_disjoint_sets(
            x = rownames(object),
            y = as.character(seq_len(nrow(object)))
        )
    }
}



# `tibble::has_rownames()` may be more consistent than
# `assertive.properties::has_rownames()` for `DataFrame` and `tbl_df` class.
#' @rdname assertHasRownames
#' @export
hasRownames <- function(object) {
    if (is(object, "tbl_df")) {
        "rowname" %in% colnames(object)
    } else if (identical(
        x = as.character(rownames(object)),
        y = as.character(seq_len(nrow(object)))
    )) {
        # Check for numeric rownames that match rows.
        FALSE
    } else {
        has_rownames(object)
    }
}
