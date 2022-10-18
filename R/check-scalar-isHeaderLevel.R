#' Does the input contain a Markdown header level?
#'
#' Markdown supports header levels `1`-`7` (`<H1>`-`<H7>`).
#'
#' @name check-scalar-isHeaderLevel
#' @note Updated 2022-10-18.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' ## TRUE ====
#' isHeaderLevel(1)
#'
#' ## FALSE ====
#' isHeaderLevel(0)
NULL


#' @rdname check-scalar-isHeaderLevel
#' @export
isHeaderLevel <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalarIntegerish(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ## Check for Markdown headers 1-7.
    ok <- isSubset(x, seq_len(7L))
    if (!isTRUE(ok)) {
        return(false(
            paste0(
                "{.var %s} is not a valid Markdown header.\n",
                "Markdown supports header levels 1-7."
            ),
            .xname
        ))
    }
    TRUE
}
