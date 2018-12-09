#' Does the Argument Contain a Markdown Header Level?
#'
#' Markdown supports header levels `1`-`7` (`<H1>`-`<H7>`).
#'
#' @name containsHeaderLevel
#' @inherit params
#' @export
#'
#' @examples
#' ## Pass ====
#' containsHeaderLevel(1)
#'
#' ## Fail ====
#' containsHeaderLevel(0)
NULL



.containsHeaderLevel <- function(x) {
    ok <- is_scalar_integerish(x)
    if (!ok) {
        return("Must be scalar integerish")
    }

    ok <- x %in% seq_len(7L)
    if (!ok) {
        return("Markdown supports header levels 1-7")
    }

    TRUE
}



#' @rdname containsHeaderLevel
#' @export
containsHeaderLevel <- makeTestFunction(.containsHeaderLevel)
