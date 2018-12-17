#' @include base-checkmate-makeTest.R



#' Does the Input Contain Elements That Are All Atomic?
#'
#' @name allAreAtomic
#' @inherit params
#'
#' @seealso `is.atomic`.
#'
#' @examples
#' ## Pass ====
#' allAreAtomic(data.frame(a = "foo", b = "bar"))
#' allAreAtomic(list(a = "foo", b = "bar"))
#'
#' ## Fail ====
#' allAreAtomic(list(a = "x", b = list()))
NULL



.allAreAtomic <- function(x) {
    ok <- all(vapply(
        X = x,
        FUN = is.atomic,
        FUN.VALUE = logical(1L)
    ))
    if (!isTRUE(ok)) {
        return("Not all elements in the object are atomic")
    }
    TRUE
}



#' @rdname allAreAtomic
#' @export
allAreAtomic <- makeTestFunction(.allAreAtomic)
