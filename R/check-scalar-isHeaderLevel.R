#' Does the input contain a Markdown header level?
#'
#' Markdown supports header levels `1`-`7` (`<H1>`-`<H7>`).
#'
#' @name check-scalar-isHeaderLevel
#' @inherit params
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
# Updated 2019-07-15.
isHeaderLevel <- function(x, .xname = getNameInParent(x)) {
    ok <- isScalarIntegerish(x)
    if (!isTRUE(ok)) return(ok)

    ok <- x %in% seq_len(7L)
    if (!isTRUE(ok)) {
        return(false(
            paste0(
                "%s is not a valid Markdown header.\n",
                "Markdown supports header levels 1-7."
            ),
            .xname
        ))
    }

    TRUE
}
