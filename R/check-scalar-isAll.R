#' Does the input belong to or inherit all of these classes?
#'
#' @inherit params
#' @export
#'
#' @examples
#' x <- 1L
#'
#' ## TRUE ====
#' isAll(x, classes = c("integer", "numeric"))
#'
#' ## FALSE ====
#' isAll(x, classes = c("integer", "NULL"))
isAll <- function(x, classes, .xname = getNameInParent(x)) {
    ok <- all(is2(x, class = classes))
    if (!isTRUE(ok)) {
        return(false(
            "%s is not all: %s",
            .xname, toString(classes)
        ))
    }
    TRUE
}
