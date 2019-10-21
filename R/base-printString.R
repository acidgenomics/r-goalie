#' Print string
#'
#' Capture [`print()`][base::print] output as a `character` string.
#'
#' Useful for returning informative messages inside a function.
#'
#' @export
#' @note Updated 2019-07-29.
#'
#' @param x An object used to select a [`print()`][base::print] method.
#' @param ... Passthrough arguments to [`print()`][base::print].
#' @param max `integer(1)`.
#'   Maximum length of vector. Works internally by calling
#'   [`head()`][utils::head] on the print capture, prior to collapse using
#'   [`paste()`][base::paste]. Supports `getOption("max.print")` global
#'   variable.
#'
#' @return `character(1)`.
#'
#' @seealso `cat()`.
#'
#' @examples
#' printString(c("hello", "world"))
#' printString(datasets::mtcars, max = 2L)
printString <- function(
    x, ...,
    max = getOption("max.print", 100L)
) {
    ## Note that this is called inside `assert()` for traceback support.
    stopifnot(isInt(max))
    x <- capture.output(print(x, ...))
    ## Limit the number of lines returned, like `max.print` option.
    x <- head(x, n = max)
    x <- paste(x, collapse = "\n")
    ## Remove leading and trailing line breaks.
    x <- gsub("^[\n]+|[\n]+$", "", x)
    x
}
