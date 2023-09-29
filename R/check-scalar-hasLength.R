#' Does the input have a non-zero or defined length?
#'
#' @name check-scalar-hasLength
#' @note Updated 2023-09-29.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @param n `NULL` or `integer`.
#' If `NULL` (default), the function will check to see if the input length is
#' non-zero.
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
hasLength <- function(x, n = NULL) {
    length <- length(x)
    if (is.null(n)) {
        if (identical(length, 0L)) {
            return(false("{.var %s} has length 0.", toCauseName(x)))
        } else {
            return(TRUE)
        }
    }
    ok <- identical(length, n)
    if (!isTRUE(ok)) {
        return(false(
            "{.var %s} doesn't have a length of %d.",
            toCauseName(x), n
        ))
    }
    TRUE
}
