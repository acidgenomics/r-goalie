#' Does the input have dimensions?
#'
#' @name check-scalar-hasDims
#' @note Updated 2023-09-19.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param n `integer` or `NULL`.
#' Expected dimension number.
#' For `hasDims`, `integer(2)` is required, corresponding to rows, columns.
#' If `NULL`, only checks for non-zero dimensions.
#'
#' @seealso
#' - `assertive.properties::has_dims()`.
#' - `assertive.properties::has_rows()`.
#' - `assertive.properties::has_cols()`.
#'
#' @examples
#' ## TRUE ====
#' x <- datasets::mtcars
#' hasDims(x)
#' hasRows(x)
#' hasCols(x)
#'
#' ## Note that dims don't have to be non-zero, just not NULL.
#' hasDims(data.frame())
#'
#' ## Expected dimension number is supported.
#' x <- matrix(data = seq(from = 1L, to = 6L), nrow = 3L, ncol = 2L)
#' ## For `hasDims`, `n` corresponds to rows, columns.
#' hasDims(x, n = c(3L, 2L))
#' hasRows(x, n = 3L)
#' hasCols(x, n = 2L)
#'
#' ## FALSE ====
#' x <- data.frame()
#' hasDims(list())
#' hasRows(x)
#' hasCols(x)
NULL



#' @rdname check-scalar-hasDims
#' @export
hasDims <- function(x, n = NULL) {
    d <- dim(x)
    if (is.null(d)) {
        return(false(
            "The dimensions of {.var %s} are {.val %s}.",
            .toName(x), "NULL"
        ))
    }
    if (!is.null(n)) {
        if (!all(d == n)) {
            return(false(
                paste(
                    "Dimension mismatch for {.var %s}:",
                    "expected {.val %s}; actual {.val %s}."
                ),
                .toName(x), deparse(n), deparse(d)
            ))
        }
    }
    TRUE
}



#' @rdname check-scalar-hasDims
#' @export
hasRows <- function(x, n = NULL) {
    nr <- nrow(x)
    if (is.null(nr)) {
        return(false(
            "The number of rows in {.var %s} is {.val %s}.",
            .toName(x), "NULL"
        ))
    }
    if (!is.null(n)) {
        if (isFALSE(nr == n)) {
            return(false(
                paste(
                    "Row number mismatch for {.var %s}:",
                    "expected {.val %s}; actual {.val %s}."
                ),
                .toName(x), n, nr
            ))
        }
    } else {
        if (identical(nr, 0L)) {
            return(false(
                "The number of rows in {.var %s} is zero.",
                .toName(x)
            ))
        }
    }
    TRUE
}



#' @rdname check-scalar-hasDims
#' @export
hasCols <- function(x, n = NULL) {
    nc <- ncol(x)
    if (is.null(nc)) {
        return(false(
            "The number of columns in {.var %s} is {.val %s}.",
            .toName(x), "NULL"
        ))
    }
    if (!is.null(n)) {
        if (isFALSE(nc == n)) {
            return(false(
                paste(
                    "Column number mismatch for {.var %s}:",
                    "expected {.val %s}; actual {.val %s}."
                ),
                .toName(x), n, nc
            ))
        }
    } else {
        if (identical(nc, 0L)) {
            return(false(
                "The number of columns in {.var %s} is zero.",
                .toName(x)
            ))
        }
    }
    TRUE
}
