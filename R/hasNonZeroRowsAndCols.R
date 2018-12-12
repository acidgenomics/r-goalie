#' Does the Input Contain Non-Zero Rows and Columns?
#'
#' Useful for quickly checking to see if we have dropped rows or columns
#' containing all zeros.
#'
#' This is a common check when handling RNA-seq data prior to generating a
#' heatmap or applying a log transformation, for example.
#'
#' Currently requires a `matrix` or `sparseMatrix` as input.
#'
#' @name hasNonZeroRowsAndCols
#' @inherit params
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
NULL



.hasNonZeroRowsAndCols <- function(object) {
    if (!isAny(object, classes = c("matrix", "sparseMatrix"))) {
        return("Must contain matrix or sparseMatrix")
    }

    if (!hasRows(object)) {
        return("Must contain rows")
    }
    if (!hasCols(object)) {
        return("Must contain columns")
    }

    # For sparse matrix, use the generic verbs from Matrix package. Note that
    # sparse matrices are often highly zero-inflated, so this approach might
    # not be generally recommended for this data class.
    if (is(object, "sparseMatrix")) {
        requireNamespace("Matrix", quietly = TRUE)
        colSums <- Matrix::colSums
        rowSums <- Matrix::rowSums
    }

    # Inform the user if any rows or columns contain all zeros. It's good
    # practice to remove them before attempting to plot a heatmap.
    zeroRows <- rowSums(object) == 0L
    if (any(zeroRows)) {
        return(paste0(
            sum(zeroRows, na.rm = TRUE),
            " rows containing all zeros detected.\n",
            toString(head(which(zeroRows)))
        ))
    }
    zeroCols <- colSums(object) == 0L
    if (any(zeroCols)) {
        return(paste0(
            sum(zeroCols, na.rm = TRUE),
            " columns containing all zeros detected.\n",
            toString(head(which(zeroCols)))
        ))
    }

    TRUE
}



#' @rdname hasNonZeroRowsAndCols
#' @export
hasNonZeroRowsAndCols <- makeTestFunction(.hasNonZeroRowsAndCols)
