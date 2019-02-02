#' Does the input contain elements that are all atomic?
#'
#' @name allAreAtomic
#' @inherit params
#' @export
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
allAreAtomic <- function(x, .xname = getNameInParent(x)) {

    # If we don't add this, the `all()` step below will return TRUE.
    ok <- hasLength(x, .xname = .xname)
    if (!isTRUE(ok)) return(ok)

    ok <- all(bapply(x, is.atomic))
    if (!isTRUE(ok)) {
        return(false("Not all elements in %s are atomic.", .xname))
    }

    TRUE
}
