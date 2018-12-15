# @seealso R.utils::capitalize.
# methods("capitalize")
# getS3method(f = "capitalize", class = "default")
capitalize <- function(str, ...) {
    n <- length(str)
    if (n == 0L) {
        return(str)
    }
    nas <- is.na(str)
    idxs <- which(nas)
    if (length(idxs) == n) {
        return(str)
    }
    res <- character(length = n)
    if (length(idxs) > 0L) {
        res[idxs] <- NA_character_
    }
    idxs <- which(!nas)
    if (length(idxs) > 0L) {
        t <- str[idxs]
        first <- substring(t, first = 1L, last = 1L)
        tail <- substring(t, first = 2L)
        first <- toupper(first)
        res[idxs] <- paste(first, tail, sep = "")
    }
    res
}
