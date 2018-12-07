#' Are These Valid Names?
#'
#' @name validNames
#' @inherit params
#' @export
#'
#' @seealso
#' - `make.names()`.
#' - `basejump::makeNames()`.
#'
#' @examples
#' ## Dots (periods) and underscores are valid.
#' validNames(c("sample.1", "sample_1"))
#'
#' ## Can't begin with a number.
#' validNames("293cells")
#'
#' ## Spaces, dashes (hyphens), and other non-alphanumerics aren't valid.
#' validNames("sample 1")
#' validNames("cell-AAAAAAAA")
#' validNames("GFP+")
validNames <- function(names) {
    if (
        !is.character(names) ||
        length(names) == 0L
    ) {
        return(FALSE)
    }
    identical(names, make.names(names, unique = TRUE))
}
