#' Does the object belong to or inherit any of these classes?
#'
#' @name check-scalar-isAny
#' @note Updated 2019-10-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @examples
#' x <- 1L
#'
#' ## TRUE ====
#' isAny(x, classes = c("integer", "NULL"))
#' isAny(x, classes = c("numeric", "NULL"))
#' isAny(x, classes = c("atomic", "NULL"))
#'
#' ## FALSE ====
#' isAny(x, classes = c("character", "data.frame"))
NULL



#' @rdname check-scalar-isAny
#' @export
isAny <- function(x, classes, .xname = getNameInParent(x)) {
    ok <- isCharacter(classes)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- any(.is2(x, class = classes))
    if (!isTRUE(ok)) {
        return(false(
            "'%s' is not any of: %s.",
            .xname, toString(classes)
        ))
    }
    TRUE
}
