#' Does the input contain a duplicate value?
#'
#' @name check-vector-isDuplicate
#' @inherit check return
#' @note Updated 2023-09-29.
#'
#' @details
#' This check is designed to serve as a complement to the base `duplicated`
#' function, which only returns `TRUE` after the first observed duplicate value.
#' Our check here returns `TRUE` for all values that are duplicated.
#'
#' @param x `vector`.
#' Any vector type (e.g. `character`, `logical`, `numeric`).
#'
#' @examples
#' ## TRUE ====
#' x <- c("a", "a", "b", "b", "c", "d")
#' isDuplicate(x)
#' duplicated(x)
#'
#' ## FALSE ====
#' isDuplicate(c("a", "b", "c"))
NULL



## Vector ======================================================================

#' @describeIn check-vector-isDuplicate Vectorized.
#' @export
isDuplicate <- function(x) {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.vector(x)
    if (!isTRUE(ok)) {
        return(false("Not vector: %s.", .toName(x)))
    }
    lgl <- duplicated(x)
    vals <- x[lgl]
    ok <- x %in% vals
    setCause(ok, false = "unique")
}
