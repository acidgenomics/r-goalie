#' Assert Is Alpha
#'
#' An alpha level must be a `numeric scalar` greater than 0 and less than 1.
#'
#' @inherit params
#' @export
#'
#' @examples
#' assertIsAlpha(0.05)
assertIsAlpha <- function(object) {
    name <- get_name_in_parent(object)
    assert_that(
        is_a_number(object),
        is_in_open_range(object, lower = 0L, upper = 1L),
        msg = paste(name, "is not a numeric scalar between 0 and 1.")
    )
}
