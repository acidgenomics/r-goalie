# FIXME Improve the error messages here.

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

.msg.areNonExisting <-  # nolint
    function(x) {
        paste(x, "contains names that already exist in the environment.")
    }

on_failure(areNonExisting) <- function(call, env) {
    .msg.areNonExisting(x = deparse(call[["x"]]))
}

#' @rdname areNonExisting
#' @export
assertAreNonExisting <- function(
    x,
    envir = parent.frame(),
    inherits = FALSE
) {
    # `assert_that()` isn't returning error as expected here because of `envir`.
    if (!areNonExisting(x = x, envir = envir, inherits = inherits)) {
        stop(.msg.areNonExisting(x = deparse(substitute(x))))
    } else {
        TRUE
    }
}
