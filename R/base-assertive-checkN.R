# `assertive.properties:::check_n()`.
checkN <- function(n) {
    if (n < 0 || n != round(n)) {
        stop("n should be a non-negative integer vector.")
    }
}
