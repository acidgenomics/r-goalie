#' Does the object belong to or inherit any of these classes?
#'
#' @inherit params
#' @export
#'
#' @examples
#' x <- 1L
#'
#' ## Pass ====
#' isAny(x, classes = c("integer", "NULL"))
#' isAny(x, classes = c("numeric", "NULL"))
#' isAny(x, classes = c("atomic", "NULL"))
#'
#' ## Fail ====
#' isAny(x, classes = c("character", "data.frame"))
isAny <- function(x, classes, .xname = getNameInParent(x)) {
    ok <- isCharacter(classes)
    if (!isTRUE(ok)) {
        return(ok)
    }

    ok <- any(is2(x, class = classes))
    if (!isTRUE(ok)) {
        return(false(
            "%s is not any of: %s.",
            .xname, toString(classes)
        ))
    }

    TRUE
}
