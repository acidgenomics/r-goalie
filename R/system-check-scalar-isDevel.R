#' Is the current session running inside R-devel?
#'
#' @name check-scalar-isDevel
#' @note Updated 2020-10-29.
#'
#' @inherit check return
#'
#' @examples
#' isDevel()
NULL



#' @rdname check-scalar-isDevel
#' @export
isDevel <- function() {
    ok <- isMatchingFixed(x = R.version.string, pattern = "development")
    if (!isTRUE(ok)) {
        return(false(
            "Not R Devel: {.val %s}.",
            as.character(R.version.string)
        ))
    }
    TRUE
}
