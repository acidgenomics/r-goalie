#' Is Integerish
#'
#' Check to see if a numeric vector is an implicit integer
#' (e.g. `1` instead of `1L`).
#'
#' @name isImplicitInteger
#' @inherit params
#'
#' @examples
#' isImplicitInteger(c(1, 2))
#' isAnImplicitInteger(1)
NULL



# isImplicitInteger ============================================================
#' @rdname isImplicitInteger
#' @export
isImplicitInteger <- is_integerish

#' @rdname isImplicitInteger
#' @export
assertIsImplicitInteger <- function(x) {
    assert_that(isImplicitInteger(x))
}

#' @rdname isImplicitInteger
#' @export
assertIsImplicitIntegerOrNULL <- function(x) {
    assert_that(any(
        isImplicitInteger(x),
        is.null(x)
    ))
}



# isAnImplicitInteger ==========================================================
#' @rdname isImplicitInteger
#' @export
isAnImplicitInteger <- is_scalar_integerish

#' @rdname isImplicitInteger
#' @export
assertIsAnImplicitInteger <- function(x) {
    assert_that(isAnImplicitInteger(x))
}

#' @rdname isImplicitInteger
#' @export
assertIsAnImplicitIntegerOrNULL <- function(x) {
    assert_that(any(
        isAnImplicitInteger(x),
        is.null(x)
    ))
}
