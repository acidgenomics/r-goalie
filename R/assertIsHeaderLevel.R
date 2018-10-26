#' Assert Is Markdown Header Level
#'
#' Markdown supports header levels 1-7 (`<H1>`-`<H7>`).
#'
#' @inherit assert
#' @export
#'
#' @examples
#' assertIsHeaderLevel(1L)
assertIsHeaderLevel <- function(object) {
    assert_is_a_number(object)
    assert_is_subset(as.integer(object), seq_len(7L))
}
