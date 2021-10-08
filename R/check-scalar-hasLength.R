#' Does the input have a non-zero or defined length?
#'
#' @name check-scalar-hasLength
#' @note Updated 2021-10-08.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
NULL



#' @rdname check-scalar-hasLength
#' @export
hasLength <- function(x, n = NULL, .xname = getNameInParent(x)) {
    length <- length(x)
    if (is.null(n)) {
        if (identical(length, 0L)) {
            return(false("{.var %s} has length 0.", .xname))
        } else {
            return(TRUE)
        }
    }
    assert(isInt(n), isTRUE(n >= 0L))
    ok <- identical(length, n)
    if (!isTRUE(ok)) {
        return(false("{.var %s} doesn't have a length of %d.", .xname, n))
    }
    TRUE
}
