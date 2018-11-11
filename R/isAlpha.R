#' Is Alpha
#'
#' An alpha level must be a `numeric scalar` greater than 0 and less than 1.
#'
#' @inherit params
#' @export
#'
#' @examples
#' assert_that(isAlpha(0.05))
isAlpha <- function(x) {
    if (!is_a_number(x)) {
        return(FALSE)
    }
    x > 0L && x < 1L
}
on_failure(isAlpha) <- function(call, env) {
    paste0(
        deparse(call[["x"]]), " is not an alpha (numeric scalar > 0 and < 1)."
    )
}
