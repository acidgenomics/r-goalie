#' Are All Items Atomic?
#'
#' @inherit params
#' @export
#'
#' @examples
#' allAreAtomic(datasets::mtcars)
allAreAtomic <- function(x) {
    all(vapply(
        X = x,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    ))
}



#' @rdname allAreAtomic
#' @export
all_are_atomic <- allAreAtomic



#' @rdname allAreAtomic
#' @export
assert
