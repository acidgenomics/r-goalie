#' Does the Variable Not Exist?
#'
#' @inherit params
#' @export
#'
#' @param x `character`. Variable names to check in `environment`.
#'
#' @examples
#' areNonExisting(c("XXX", "YYY"))
areNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = TRUE
) {
    all(!is_existing(x, envir = envir, inherits = inherits))
}

#' @rdname areNonExisting
#' @export
assertAreNonExisting <- function(...) {
    assert_that(areNonExisting(...))
}
