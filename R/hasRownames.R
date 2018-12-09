#' Does the Object Have Row Names?
#'
#' @section data.frame:
#'
#' Standard `data.frame` class objects cannot have `NULL` row names defined.
#' Here we are checking to see if a `data.frame` has soft `NULL` row names,
#' meaning that they return as a sequence that is identical to the number of
#' rows.
#'
#' @name hasRownames
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
#' hasRownames(x)
#'
#' ## Fail ====
#' x <- data.frame(a = seq_len(2L))
#' print(x)
#' # Standard data frame doesn't allow NULL row names.
#' rownames(x)
#' hasRownames(x)
#'
#' x <- S4Vectors::DataFrame(a = seq_len(2L))
#' print(x)
#' # S4 data frame does allow NULL row names.
#' rownames(x)
#' hasRownames(x)
NULL



.hasRownames <- function(x) {
    # Classes that extend data.frame but intentionally don't support row names.
    if (is(x, "data.table")) {
        return("data.table class objects don't support row names")
    } else if (is(x, "tbl_df")) {
        return("tibble (tbl_df) class objects don't support row names")
    }

    # Standard data frames can't return NULL row names, so check for sequence.
    if (
        is(x, "data.frame") &&
        identical(
            x = as(rownames(x), "character"),
            y = as(seq_len(nrow(x)), "character")
        )
    ) {
        return("Object is data.frame with sequence row names (soft NULL)")
    }

    # Other classes (e.g. matrix, DataFrame) do support NULL row names.
    ok <- !is.null(rownames(x))
    if (!ok) {
        "Object has NULL row names"
    }

    TRUE
}



#' @rdname hasRownames
#' @export
hasRownames <- makeTestFunction(.hasRownames)
