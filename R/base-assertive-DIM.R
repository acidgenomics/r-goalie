# `assertive.properties::DIM()`.
DIM <- function(x) {
    dim_x <- dim(x)
    if (is.null(dim_x)) {
        length(x)
    } else {
        dim_x
    }
}
