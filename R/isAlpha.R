#' Is Alpha
#'
#' An alpha level must be a `numeric scalar` greater than 0 and less than 1.
#'
#' @inherit params
#' @export
#'
#' @examples
#' isAlpha(0.05)
isAlpha <- function(x) {
    if (!is_scalar_double(x)) {
        return(FALSE)
    }
    x > 0L && x < 1L
}

.msg.isAlpha <- function(x) {
    paste(x, "is not an alpha (numeric scalar > 0 and < 1).")
}

on_failure(isAlpha) <- function(call, env) {
    .msg.isAlpha(x = deparse(call[["x"]]))
}

#' @rdname isAlpha
#' @export
assertIsAlpha <- function(x) {
    assert_that(
        isAlpha(x),
        msg = .msg.isAlpha(x = deparse(substitute(x)))
    )
}
