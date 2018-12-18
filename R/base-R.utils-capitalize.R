#' Capitalize
#'
#' Capitalize each character string in a vector.
#'
#' @keywords internal
#'
#' @param x `vector` of `character` strings to be capitalized.
#'
#' @seealso
#' `R.utils::capitalize()`.
#'
#' ```
#' getS3method(
#'     f = "capitalize",
#'     class = "default",
#'     envir = asNamespace("R.utils")
#' )
#' ```
#'
#' @examples
#' capitalize(c("hello", "world"))
capitalize <- function(x) {
    # Using `R.utils::capitalize()` method here.
    n <- length(x)
    if (n == 0L) {
        return(x)
    }
    nas <- is.na(x)
    idxs <- which(nas)
    if (length(idxs) == n) {
        return(x)
    }
    res <- character(length = n)
    if (length(idxs) > 0L) {
        res[idxs] <- NA_character_
    }
    idxs <- which(!nas)
    if (length(idxs) > 0L) {
        t <- x[idxs]
        first <- substring(t, first = 1L, last = 1L)
        tail <- substring(t, first = 2L)
        first <- toupper(first)
        res[idxs] <- paste(first, tail, sep = "")
    }
    res
}
