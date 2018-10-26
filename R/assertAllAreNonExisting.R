#' Assert All Variables Are Non-Existing
#'
#' @inherit assert
#' @export
#'
#' @param object `character`. Variable names to check in `environment`.
#'
#' @examples
#' assertAllAreNonExisting(c("XXX", "YYY"))
assertAllAreNonExisting <- function(
    object,
    envir = parent.frame(),
    inherits = FALSE
) {
    exists <- is_existing(object, envir = envir, inherits = inherits)
    if (any(exists)) {
        stop(paste(
            "Already exists in environment:",
            toString(object[exists])
        ))
    }
}
