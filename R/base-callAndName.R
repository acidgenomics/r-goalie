# `assertive.base:::to_names()`.
toNames <- function(x) {
    if (is.double(x) && is.vector(x)) {
        ifelse(
            test = is.na(x),
            yes = NA_real_,
            no = sprintf("%.17g", x)
        )
    }
    else if (is.complex(x)) {
        ifelse(
            test = is.na(x),
            yes = NA_complex_,
            no = sprintf("%.17g+%.17gi", Re(x), Im(x))
        )
    }
    else {
        as.character(x)
    }
}



# `assertive.base::call_and_name()`.
callAndName <- function(fn, x, ...) {
    y <- fn(x, ...)
    dim(y) <- dim(x)
    names(y) <- toNames(x)
    y
}
