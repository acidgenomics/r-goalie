#' Does the input contain non-zero rows and columns?
#'
#' Useful for quickly checking to see if we have dropped rows or columns
#' containing all zeros.
#'
#' This is a common check when handling RNA-seq data prior to generating a
#' heatmap or applying a log transformation, for example.
#'
#' @name check-scalar-hasNonzeroRowsAndCols
#' @note Updated 2021-01-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' x <- matrix(data = seq_len(4L), nrow = 2L)
#' print(x)
#' hasNonzeroRowsAndCols(x)
#'
#' ## FALSE ====
#' x <- matrix(data = rep(c(0L, 1L), times = 2L), nrow = 2L, byrow = FALSE)
#' print(x)
#' hasNonzeroRowsAndCols(x)
#'
#' x <- matrix(data = rep(c(0L, 1L), times = 2L), nrow = 2L, byrow = TRUE)
#' print(x)
#' hasNonzeroRowsAndCols(x)
NULL



#' @rdname check-scalar-hasNonzeroRowsAndCols
#' @export
hasNonzeroRowsAndCols <- function(x, .xname = getNameInParent(x)) {
    ok <- isAny(x = x, classes = c("matrix", "Matrix"), .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- hasRows(x, .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- hasCols(x, .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    if (is(x, "Matrix")) {
        requireNamespaces("Matrix")
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
                msg1 = "{.var %s} has %s zero row at position %s.",
                msg2 = "{.var %s} has %s zero rows at positions %s."
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
                msg1 = "{.var %s} has %s zero column at position %s.",
                msg2 = "{.var %s} has %s zero columns at positions %s."
            ),
            .xname, n, which
        ))
    }
    TRUE
}
