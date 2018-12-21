# @seealso `R.utils::capitalize()`.
.capitalize <- function(x) {
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
