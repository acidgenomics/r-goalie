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
are_non_existing <- areNonExisting



#' @rdname areNonExisting
#' @export
assertAreNonExisting <- makeAssertionFunction(areNonExisting)

#' @rdname areNonExisting
#' @export
assert_are_non_existing <- assertAreNonExisting
