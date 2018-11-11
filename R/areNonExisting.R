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
assertAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = FALSE
) {
    # `assert_that()` isn't returning error correctly here.
    stopifnot(areNonExisting(
        x = x,
        envir = envir,
        inherits = inherits
    ))
}
