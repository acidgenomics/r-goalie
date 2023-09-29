#' Does the input contain elements that are all atomic?
#'
#' @name check-scalar-allAreAtomic
#' @note Updated 2023-09-29.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso `is.atomic()`.
#'
#' @examples
#' ## TRUE ====
#' allAreAtomic(data.frame(a = "foo", b = "bar"))
#' allAreAtomic(list(a = "foo", b = "bar"))
#'
#' ## FALSE ====
#' allAreAtomic(data.frame())
#' allAreAtomic(list(a = "x", b = list()))
NULL



#' @rdname check-scalar-allAreAtomic
#' @export
allAreAtomic <- function(x) {
    ok <- hasLength(x)
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- all(bapply(x, is.atomic))
    if (!isTRUE(ok)) {
        return(false(
            "Not all elements in {.var %s} are atomic.",
            toCauseName(x)
        ))
    }
    TRUE
}
