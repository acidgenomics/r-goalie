#' Get the Name of a Variable in the Parent Frame
#'
#' @export
#'
#' @param x `symbol`.
#'   Variable to get the name of.
#' @param escapePercent `logical(1)`.
#'   If `TRUE`, percent signs are doubled, making the value suitable for use
#'   with `sprintf`.
#'
#' @seealso
#' - `assertive.base::get_name_in_parent`.
#' - `checkmate::vname`.
#'
#' @examples
#' getNameInParent(test)
getNameInParent <- function(x, escapePercent = TRUE) {
    xname <- safeDeparse(do.call(
        what = substitute,
        args = list(substitute(x), parent.frame())
    ))
    if (isTRUE(escapePercent)) {
        xname <- gsub("%", "%%", xname)
    }
    xname
}



#' @rdname getNameInParent
#' @export
vname <- getNameInParent
