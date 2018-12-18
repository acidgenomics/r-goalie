#' Are these valid names?
#'
#' @export
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
validNames <- function(x, .xname = getNameInParent(x)) {
    if (
        !is.character(x) ||
        length(x) == 0L
    ) {
        return(false("%s is not a non-empty character.", .xname))
    }

    ok <- identical(x, make.names(x, unique = TRUE))
    if (!isTRUE(ok)) {
        return(false(
            paste0(
                "%s does not contain valid names.\n",
                "See make.names() for details."
            ),
            .xname
        ))
    }

    TRUE
}
