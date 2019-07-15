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
#' ## TRUE ====
#' hasLength(1L)
#' hasLength(FALSE)
#' hasLength(datasets::mtcars)
#' hasLength("")
#'
#' ## FALSE ====
#' hasLength(NULL)
#' hasLength(character())
#' hasLength(data.frame())

# Updated 2019-07-15.
hasLength <- function(x, n = NULL, .xname = getNameInParent(x)) {
    length <- length(x)

    if (length == 0L) {
        return(false("%s has length 0.", .xname))
    }

    if (is.null(n)) {
        return(TRUE)
    } else {
        assert(isInt(n), n > 0L)
    }

    ok <- length == n
    if (!isTRUE(ok)) {
        return(false("%s does not have a length of %d.", .xname, n))
    }

    TRUE
}
