# FIXME

#' Look up a variable name
#'
#' Tries to heuristically determine the variable name of `x` in the
#' `parent.frame` with a combination of `deparse` and `substitute`.
#'
#' @inheritParams params
#'
#' @return `character(1)`.
#' Variable name.
#'
#' @seealso checkmate::vname.
#'
#' @examples
#' test <- 1L
#' vname(test)
vname <- function(x) {
    paste0(
        deparse(substitute(x, parent.frame(1L)), width.cutoff = 500),
        collapse = "\n"
    )
}
