#' Are these valid names?
#'
#' @name check-scalar-validNames
#' @note Updated 2019-08-09.
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

    setdiff <- setdiff(x, make.names(x, unique = TRUE))
    if (hasLength(setdiff)) {
        return(false(
            paste0(
                "'%s' does not contain valid names.\n",
                "See 'make.names()' for details.\n",
                "Invalid: %s"
            ),
            .xname,
            toString(setdiff, width = 200L)
        ))
    }

    TRUE
}
