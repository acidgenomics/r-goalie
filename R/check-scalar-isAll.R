#' Does the input belong to or inherit all of these classes?
#'
#' @name check-scalar-isAll
#' @note Updated 2019-10-04.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
isAll <- function(x, classes) {
    ok <- all(.is2(x, class = classes))
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} is not all of: %s.",
            toCauseName(x), toString(classes, width = 50L)
        ))
    }
    TRUE
}
