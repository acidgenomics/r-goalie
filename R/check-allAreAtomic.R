#' Does the input contain elements that are all atomic?
#'
#' @name allAreAtomic
#' @inherit params
#' @export
#'
#' @seealso `is.atomic()`.
#'
#' @examples
#' ## Pass ====
#' allAreAtomic(data.frame(a = "foo", b = "bar"))
#' allAreAtomic(list(a = "foo", b = "bar"))
#'
#' ## Fail ====
#' allAreAtomic(data.frame())
#' allAreAtomic(list(a = "x", b = list()))
allAreAtomic <- function(x, .xname = getNameInParent(x)) {

    # If we don't add this, the `all()` step below will return TRUE.
    # TODO Switch to hasLength?
    if (length(x) == 0L) {
        return(false("%s has length 0.", .xname))
    }

    ok <- all(bapply(x, is.atomic))
    if (!isTRUE(ok)) {
        return(false("Not all elements in %s are atomic.", .xname))
    }

    TRUE
}
