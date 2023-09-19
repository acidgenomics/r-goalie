#' Does the input contain a duplicate value?
#'
#' @name check-vector-isDuplicate
#' @note Updated 2023-09-19.
#'
#' @details
#' This check is designed to serve as a complement to the base `duplicated`
#' function, which only returns `TRUE` after the first observed duplicate value.
#' Our check here returns `TRUE` for all values that are duplicated.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
    ## Allowing NA here, so don't use `isCharacter` check.
    ok <- is.character(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    x <- unname(x)
    lgl <- duplicated(x)
    if (!any(lgl)) {
        return(setCause(lgl, false = "unique"))
    }
    vals <- x[lgl]
    ok <- x %in% vals
    setCause(ok, false = "unique")
}
