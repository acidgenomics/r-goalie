#' Does the input contain non-zero rows and columns?
#'
#' Useful for quickly checking to see if we have dropped rows or columns
#' containing all zeros.
#'
#' This is a common check when handling RNA-seq data prior to generating a
#' heatmap or applying a log transformation, for example.
#'
#' @export
#' @inherit params
#'
#' @param x Matrix.
#'   Currently requires a `matrix` or `sparseMatrix` as input.
#
#' @examples
#' ## Pass ====
#' x <- matrix(data = seq_len(4), nrow = 2)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' ## Fail ====
#' x <- matrix(nrow = 0, ncol = 0)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' x <- matrix(data = rep(0L, times = 2), byrow = FALSE)
#' print(x)
#' hasNonZeroRowsAndCols(x)
#'
#' x <- matrix(data = rep(0L, times = 2), byrow = TRUE)
#' print(x)
#' hasNonZeroRowsAndCols(x)
hasNonZeroRowsAndCols <- function(x, .xname = getNameInParent(x)) {
    ok <- isAny(x = x, classes = c("matrix", "sparseMatrix"), .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- hasRows(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- hasRows(x, .xname = .xname)
    if (!isTRUE(x)) return(ok)

    # For sparse matrix, use the generic verbs from Matrix package. Note that
    # sparse matrices are often highly zero-inflated, so this approach might
    # not be generally recommended for this data class.
    if (is(x, "sparseMatrix")) {
        requireNamespace("Matrix", quietly = TRUE)
        colSums <- Matrix::colSums
        rowSums <- Matrix::rowSums
    }

    # Inform the user if any rows or columns contain all zeros. It's good
    # practice to remove them before attempting to plot a heatmap.
    zeroRows <- rowSums(x) == 0L
    if (any(zeroRows)) {
        return(false(paste0(
            sum(zeroRows, na.rm = TRUE),
            " rows containing all zeros detected.\n",
            toString(head(which(zeroRows)))
        )))
    }
    zeroCols <- colSums(x) == 0L
    if (any(zeroCols)) {
        return(false(paste0(
            sum(zeroCols, na.rm = TRUE),
            " columns containing all zeros detected.\n",
            toString(head(which(zeroCols)))
        )))
    }

    TRUE
}
