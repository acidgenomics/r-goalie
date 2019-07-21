#' Does the input contain non-zero rows and columns?
#'
#' Useful for quickly checking to see if we have dropped rows or columns
#' containing all zeros.
#'
#' This is a common check when handling RNA-seq data prior to generating a
#' heatmap or applying a log transformation, for example.
#'
#' @name check-scalar-hasNonZeroRowsAndCols
#' @inherit params
#'
#' @param x Matrix.
#'   Currently requires a `matrix` or `sparseMatrix` as input.
## #' @examples
#' ## TRUE ====
#' x <- matrix(data = seq_len(4), nrow = 2)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' x <- matrix(data = rep(1, times = 2), byrow = TRUE)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' x <- matrix(data = rep(1, times = 2), byrow = FALSE)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' ## FALSE ====
#' x <- matrix(nrow = 0, ncol = 0)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' x <- matrix(nrow = 1, ncol = 0)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' x <- matrix(nrow = 0, ncol = 1)
#' print(x)
#' hasNonZeroRowsAndCols(x)
NULL



#' @rdname check-scalar-hasNonZeroRowsAndCols
#' @export
## Updated 2019-07-15.
hasNonZeroRowsAndCols <- function(x, .xname = getNameInParent(x)) {
    ok <- isAny(x = x, classes = c("matrix", "sparseMatrix"), .xname = .xname)
    if (!isTRUE(ok)) return(ok)  # nocov

    ok <- hasRows(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- hasCols(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ## For sparse matrix, use the generic verbs from Matrix package. Note that
    ## sparse matrices are often highly zero-inflated, so this approach might
    ## not be generally recommended for this data class.
    if (is(x, "sparseMatrix")) {
        requireNamespace("Matrix", quietly = TRUE)
        colSums <- Matrix::colSums
        rowSums <- Matrix::rowSums
    }

    ## Inform the user if any rows or columns contain all zeros. It's good
    ## practice to remove them before attempting to plot a heatmap.
    zeroRows <- rowSums(x) == 0L
    if (any(zeroRows)) {
        n <- sum(zeroRows, na.rm = TRUE)
        which <- toString(x = head(which(zeroRows)), width = 100L)
        return(false(
            ngettext(
                n = n,
                msg1 = "%s has %s zero row at position %s.",
                msg2 = "%s has %s zero rows at positions %s."
            ),
            .xname, n, which
        ))
    }
    zeroCols <- colSums(x) == 0L
    if (any(zeroCols)) {
        n <- sum(zeroCols, na.rm = TRUE)
        which <- toString(head(which(zeroCols)), width = 100L)
        return(false(
            ngettext(
                n = n,
                msg1 = "%s has %s zero column at position %s.",
                msg2 = "%s has %s zero columns at positions %s."
            ),
            .xname, n, which
        ))
    }

    TRUE
}
