#' Get the name of a variable defined in the parent frame
#'
#' @export
#' @note Updated 2019-09-06.
#'
#' @param x `symbol`.
#'   Variable to get the name of.
#' @param escapePercent `logical(1)`.
#'   If `TRUE`, percent signs are doubled, making the value suitable for use
#'   with [`sprintf()`][base::sprintf].
#' @param width `integer(1)`.
#'   Maximum width of the name.
#'
#' @seealso
#' - `assertive.base::get_name_in_parent()`.
#' - `checkmate::vname()`.
#'
#' @return `character(1)`.
#'
#' @examples
#' getNameInParent(test)
getNameInParent <- function(x, escapePercent = TRUE, width = 100L) {
    xname <- safeDeparse(do.call(
        what = substitute,
        args = list(substitute(x), parent.frame())
    ))
    if (isTRUE(escapePercent)) {
        xname <- gsub("%", "%%", xname)
    }
    toString(xname, width = width)
}
