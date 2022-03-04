#' Is dark mode preferred?
#'
#' @export
#' @note Updated 2022-03-04.
#'
#' @details
#' Checks for `acid.dark` option in current session.
#'
#' Useful for changing the appearance of ggplot2 output, especially for single
#' cell RNA-seq clustering plots.
#'
#' @return `logical(1)`.
#'
#' @examples
#' ## TRUE ====
#' options("acid.dark" = TRUE)
#' isDark()
#'
#' ## FALSE ====
#' options("acid.dark" = NULL)
#' isDark()
isDark <- function() {
    ok <- isTRUE(getOption(x = "acid.dark"))
    if (!isTRUE(ok)) {
        return(false("Dark mode is not enabled."))
    }
    return(TRUE)
}
