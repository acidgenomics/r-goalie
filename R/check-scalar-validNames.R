#' Are these valid names?
#'
#' @name check-scalar-validNames
#' @note Updated 2019-07-29.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @seealso
#' - `make.names()`.
#' - `basejump::makeNames()`.
#'
#' @examples
#' ## TRUE ====
#' ## Dots (periods) and underscores are valid.
#' validNames(c("sample.1", "sample_1"))
#'
#' ## FALSE ====
#' ## Can't begin with a number.
#' validNames("293cells")
#'
#' ## Spaces, dashes (hyphens), and other non-alphanumerics aren't valid.
#' validNames("sample 1")
#' validNames("cell-AAAAAAAA")
#' validNames("GFP+")
NULL



#' @rdname check-scalar-validNames
#' @export
validNames <- function(x, .xname = getNameInParent(x)) {
    ok <- isCharacter(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)  # nocov

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
