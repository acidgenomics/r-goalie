#' Is Markdown Header Level?
#'
#' Markdown supports header levels 1-7 (`<H1>`-`<H7>`).
#'
#' @inherit params
#' @export
#'
#' @examples
#' isHeaderLevel(1L)
isHeaderLevel <- function(x) {
    if (!is_scalar_integerish(x)) {
        return(FALSE)
    }
    x %in% seq_len(7L)
}

.msg.isHeaderLevel <-  # nolint
    function(x) {
        paste(x, "is not a valid Markdown header level (1-7).")
    }

on_failure(isHeaderLevel) <- function(call, env) {
    .msg.isHeaderLevel(x = deparse(call[["x"]]))
}

#' @rdname isHeaderLevel
#' @export
assertIsHeaderLevel <- function(x) {
    assert_that(
        isHeaderLevel(x),
        msg = .msg.isHeaderLevel(x = deparse(substitute(x)))
    )
}
