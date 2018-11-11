#' Does the Variable Not Exist?
#'
#' @name existing
#' @inherit params
#'
#' @param x `character`. Variable names to check in `environment`.
#'
#' @examples
#' assertAllAreNonExisting(c("XXX", "YYY"))
assertAllAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = TRUE
) {
    assert_that(all(!is_existing(x, envir = envir, inherits = inherits)))
}
