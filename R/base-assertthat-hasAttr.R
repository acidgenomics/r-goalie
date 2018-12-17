# @seealso `assertthat:::has_attr()`.
hasAttr <- function(x, which) {
    !is.null(attr(x, which, exact = TRUE))
}
