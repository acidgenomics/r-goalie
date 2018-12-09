#' Are These Valid Names?
#'
#' @name validNames
#' @inherit params
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
NULL



.validNames <- function(names) {
    if (
        !is.character(names) ||
        length(names) == 0L
    ) {
        return("Must contain non-empty character")
    }

    ok <- identical(names, make.names(names, unique = TRUE))
    if (!ok) {
        return(paste(
            "Not all names are valid in R.",
            "See make.names() documentation for details on valid names."
        ))
    }

    TRUE
}



#' @rdname validNames
#' @export
validNames <- function(x) {
    isTRUE(.validNames(x))
}
