#' Assert Is Implicit Integer
#'
#' @name assertIsImplicitInteger
#' @inherit params
#'
#' @examples
#' isAnImplicitInteger(1)
#' assertIsAnImplicitInteger(1)
#'
#' isImplicitInteger(c(1, 2))
#' assertIsImplicitInteger(c(1, 2))
NULL



#' @rdname assertIsImplicitInteger
#' @export
isAnImplicitInteger <- is_scalar_integerish



#' @rdname assertIsImplicitInteger
#' @export
isImplicitInteger <- is_integerish



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitInteger <- function(object) {
    assert_that(isAnImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsAnImplicitIntegerOrNULL <- function(object) {
    assert_that(any(
        isAnImplicitInteger(object),
        is.null(object)
    ))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitInteger <- function(object) {
    assert_that(isImplicitInteger(object))
}



#' @rdname assertIsImplicitInteger
#' @export
assertIsImplicitIntegerOrNULL <- function(object) {
    assert_that(any(
        isImplicitInteger(object),
        is.null(object)
    ))
}
