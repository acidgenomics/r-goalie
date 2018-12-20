# FIXME Get rid of the rlang dependency here...this gets called a lot.



#' Is the input integer(ish)?
#'
#' Check for valid input of either explicit (e.g. `1L`) and/or implict
#' (e.g. `1`) `integer`.
#'
#' @export
#' @inherit params
#'
#' @seealso
#' - `rlang::is_integerish()`.
#' - `checkmate::checkIntegerish()`.
#'
#' @examples
#' isIntegerish(seq_len(2L))
#' isIntegerish(c(1, 2))
isIntegerish <- function(x, .xname = getNameInParent(x)) {
    if (is.integer(x)) {
        return(TRUE)
    }
    if (!is.numeric(x)) {
        return(false("%s is not numeric.", .xname))
    }
    bapply(
        x = x,
        predicate = function(x) {
            isTRUE(all.equal(
                target = as.integer(x),
                current = x,
                tolerance = .Machine[["double.eps"]]
            ))
        }
    )
}
