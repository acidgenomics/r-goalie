#' Assert Is Implicit Integer
#'
#' @name assertIsImplicitInteger
#' @inherit assert
NULL



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitInteger(1)
assertIsAnImplicitInteger <- function(object) {
    assert_all_are_true(isAnImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsAnImplicitIntegerOrNULL(1)
#' assertIsAnImplicitIntegerOrNULL(NULL)
assertIsAnImplicitIntegerOrNULL <- function(object) {
    assert_all_are_true(any(isAnImplicitInteger(object), is.null(object)))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitInteger(c(1, 2))
assertIsImplicitInteger <- function(object) {
    assert_all_are_true(isImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' assertIsImplicitIntegerOrNULL(c(1, 2))
#' assertIsImplicitIntegerOrNULL(NULL)
assertIsImplicitIntegerOrNULL <- function(object) {
    assert_all_are_true(any(isImplicitInteger(object), is.null(object)))
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isAnImplicitInteger(1)
isAnImplicitInteger <- function(object) {
    if (!is_a_number(object)) {
        return(FALSE)
    }
    isImplicitInteger(object)
}



#' @rdname assertIsImplicitInteger
#' @export
#' @examples
#' isImplicitInteger(list(1, 1L, 1.1, "XXX"))
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
