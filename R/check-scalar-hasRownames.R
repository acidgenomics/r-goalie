#' Does the input have row names?
#'
#' @section data.frame:
#'
#' Standard `data.frame` class objects cannot have `NULL` row names defined.
#' Here we are checking to see if a `data.frame` has soft `NULL` row names,
#' meaning that they return as a sequence that is identical to the number of
#' rows.
#'
#' @name check-scalar-hasRownames
#' @note Updated 2019-12-09.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @examples
#' ## TRUE ====
#' x <- data.frame(
#'     "sample1" = c(1L, 2L),
#'     "sample2" = c(3L, 4L),
#'     row.names = c("gene1", "gene2")
#' )
#' print(x)
#' hasRownames(x)
#'
#' ## FALSE ====
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



#' @rdname check-scalar-hasRownames
#' @export
hasRownames <- function(x, .xname = getNameInParent(x)) {
    ## Classes that extend data.frame but intentionally don't support row names.
    if (inherits(x, "data.table")) {
        return(false("data.table class doesn't support row names."))
    } else if (inherits(x, "tbl_df")) {
        return(false("tbl_df class doesn't support row names."))
    }
    ## Early return if `rownames()` function fails.
    rownames <- tryCatch(
        expr = rownames(x),
        error = function(e) e
    )
    if (is(rownames, "error")) {
        return(false("'rownames()' command on '%s' failed.", .xname))  # nocov
    }
    ## Standard data frames can't return NULL row names, so check against
    ## integer comparison. Previously this used a `seq_len()` comparison
    ## approach, which will incorrectly return TRUE for subset data frames.
    if (
        is.data.frame(x) &&
        allAreMatchingRegex(x = rownames, pattern = "^[0-9]+$")
    ) {
        return(false("'%s' has integer row names (soft NULL).", .xname))
    }
    ## Other classes (e.g. matrix, DataFrame) do support NULL row names.
    ok <- !is.null(rownames)
    if (!isTRUE(ok)) {
        return(false("'%s' has NULL row names.", .xname))
    }
    TRUE
}
