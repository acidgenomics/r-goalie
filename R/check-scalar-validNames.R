#' Are these valid names?
#'
#' @name check-scalar-validNames
#' @note Updated 2019-08-11.
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
    valid <- mapply(
        x = x,
        y = make.names(x, unique = TRUE),
        FUN = identical,
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
    )
    if (!all(valid)) {
        pos <- which(!valid)
        name <- x[pos]
        info <- paste0("[", pos, "] ", name)
        return(false(
            paste0(
                "'%s' does not contain valid names: %s\n",
                "See 'make.names()' for details."
            ),
            .xname,
            toString(info, width = 100L)
        ))
    }
    TRUE
}
