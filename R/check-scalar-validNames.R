#' Are these valid names?
#'
#' @name check-scalar-validNames
#' @note Updated 2023-09-29.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `make.names()`.
#'
#' @examples
#' ## TRUE ====
#' ## Dots (periods) and underscores are valid.
#' validNames(c("sample.1", "sample_1"))
#'
#' ## FALSE ====
#' ## Can't begin with a number.
#' validNames("293cells")
#' ## Cannot contain duplicates.
#' validNames(c("a", "a"))
#' ## Spaces, dashes (hyphens), and other non-alphanumerics aren't valid.
#' validNames("sample 1")
#' validNames("cell-AAAAAAAA")
#' validNames("GFP+")
NULL



#' @rdname check-scalar-validNames
#' @export
validNames <- function(x) {
    ok <- isCharacter(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- hasNoDuplicates(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    valid <- unlist(Map(
        f = identical,
        x = x,
        y = make.names(x, unique = TRUE)
    ))
    if (!all(valid)) {
        pos <- which(!valid)
        name <- x[pos]
        info <- paste0("[", pos, "] ", name)
        return(false(
            paste0(
                "{.var %s} doesn't contain valid names: %s\n",
                "See {.fun %s} for details."
            ),
            .toName(x), toString(info, width = 100L), "make.names"
        ))
    }
    TRUE
}
