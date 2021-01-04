#' Shorten (truncate) input to a specified width
#'
#' Automatically generate a substring and add trailing "..." if necessary.
#'
#' Note that return matches the desired width.
#'
#' @name engine-shorten
#' @note Updated 2020-01-04.
#'
#' @param x `atomic`.
#'
#' @seealso `assertive.base:::truncate()`.
#'
#' @return `character`.
#'
#' @examples
#' x <- "the quick brown fox"
#' x <- shorten(x, width = 10L)
#' nchar(x)
NULL



#' @rdname engine-shorten
#' @export
shorten <- function(x, width = getOption("width")) {
    stopifnot(
        is.atomic(x),
        isInt(width),
        isTRUE(width > 3L)
    )
    x <- as.character(x)
    ifelse(
        test = nchar(x) > width,
        yes = paste0(substring(x, 1L, width - 3L), "..."),
        no = x
    )
}
