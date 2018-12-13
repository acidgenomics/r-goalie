#' Does the Input Have a Non-Zero or Defined Length?
#'
#' @name hasLength
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
NULL



.hasLength <- function(x, n = NULL) {
    length <- length(x)
    if (length == 0L) {
        return(FALSE)
    }
    if (is.null(n)) {
        return(length > 0L)
    }
    assert(isInt(n), n > 0L)
    length == n
}



#' @rdname hasLength
#' @export
hasLength <- makeTestFunction(.hasLength)
