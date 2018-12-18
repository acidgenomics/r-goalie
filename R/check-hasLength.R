#' Does the input have a non-zero or defined length?
#'
#' @export
#' @inherit params
#'
#' @param n `NULL` or `integer`.
#'   If `NULL` (default), the function will check to see if the input length is
#'   non-zero.
#'
#' @examples
#' ## Pass ====
#' hasLength(1L)
#' hasLength(FALSE)
#' hasLength(datasets::mtcars)
#' hasLength("")
#'
#' ## Fail ====
#' hasLength(NULL)
#' hasLength(character())
#' hasLength(data.frame())
hasLength <- function(x, n = NULL) {
    xname <- getNameInParent(x)
    length <- length(x)

    if (length == 0L) {
        return(false("%s has length 0.", xname))
    }

    if (is.null(n)) {
        return(length > 0L)
    }
    assert(isInt(n), n > 0L)

    ok <- length == n
    if (!isTRUE(ok)) {
        return(false("%s does not have a length of %d.", xname, n))
    }

    TRUE
}
