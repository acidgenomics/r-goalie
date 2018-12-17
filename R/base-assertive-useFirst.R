# @seealso `assertive.base::use_first()`.
useFirst <- function(
    x,
    indexer = c("[[", "["),
    .xname = getNameInParent(x)
) {
    len_x <- length(x)
    if (len_x == 0L) {
        stop(sprintf("%s has length 0.", .xname))
    }
    if (len_x == 1L) {
        return(x)
    }
    indexer <- match.fun(match.arg(indexer))
    x1 <- indexer(x, 1L)
    warning(sprintf(
        "Only the first value of %s (= %s) will be used.",
        .xname,
        as.character(x1)
    ), call. = FALSE)
    x1
}
