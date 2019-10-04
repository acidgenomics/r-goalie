#' Does the input belong to or inherit all of these classes?
#'
#' @name check-scalar-isAll
#' @note Updated 2019-10-04.
#'
#' @inherit check
#' @inheritParams acidroxygen::params
#'
#' @examples
#' x <- 1L
#'
#' ## TRUE ====
#' isAll(x, classes = c("integer", "numeric"))
#'
#' ## FALSE ====
#' isAll(x, classes = c("integer", "NULL"))
NULL



#' @rdname check-scalar-isAll
#' @export
isAll <- function(x, classes, .xname = getNameInParent(x)) {
    ok <- all(.is2(x, class = classes))
    if (!isTRUE(ok)) {
        return(false(
            "'%s' is not all of: %s.",
            .xname, toString(classes)
        ))
    }
    TRUE
}
