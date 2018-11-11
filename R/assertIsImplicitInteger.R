#' Assert Is Implicit Integer
#'
#' @name assertIsImplicitInteger
#' @inherit params
#'
#' @examples
#' isImplicitInteger(list(1, 1L, 1.1, "XXX"))
#' assertIsImplicitInteger(c(1, 2))
#' assertIsImplicitIntegerOrNULL(c(1, 2))
#' assertIsImplicitIntegerOrNULL(NULL)
#'
#' isAnImplicitInteger(1)
#' assertIsAnImplicitInteger(1)
#' assertIsAnImplicitIntegerOrNULL(1)
#' assertIsAnImplicitIntegerOrNULL(NULL)
NULL



#' @rdname assertIsImplicitInteger
#' @export
isImplicitInteger <- function(object) {
    vapply(
        X = object,
        FUN = function(object) {
            if (!is.numeric(object)) {
                return(FALSE)
            }
            if (is.integer(object)) {
                return(TRUE)
            }
            isTRUE(all.equal(
                target = as.integer(object),
                current = object,
                tolerance = .Machine[["double.eps"]]
            ))
        },
        FUN.VALUE = logical(1L)
    )
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitInteger <- function(object) {
    assert_all_are_true(isImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitIntegerOrNULL <- function(object) {
    assert_any_are_true(c(
        isImplicitInteger(object),
        is.null(object)
    ))
}



#' @rdname assertIsImplicitInteger
#' @export
isAnImplicitInteger <- function(object) {
    if (!is_a_number(object)) {
        return(FALSE)
    }
    isImplicitInteger(object)
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitInteger <- function(object) {
    assert_that(isAnImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitIntegerOrNULL <- function(object) {
    assert_that(any(isAnImplicitInteger(object), is.null(object)))
}
