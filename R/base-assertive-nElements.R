# `assertive.properties:::n_elements()`.
nElements <- function(x) {
    if (is.recursive(x)) {
        sum(vapply(x, nElements, integer(1L)))
    }
    else {
        as.integer(prod(DIM(x)))
    }
}
