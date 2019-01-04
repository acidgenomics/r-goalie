# @seealso `base::stopifnot()`.
.Dparse <-  # nolint
    function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) {
            paste(ch[[1L]], "....")
        } else {
            ch
        }
    }



# `assertive.properties:::check_n()`.
.checkN <- function(n) {
    if (n < 0L || n != round(n)) {
        stop("n should be a non-negative integer vector.")
    }
}



# Using primary assay here.
.coerceSummarizedExperimentToMatrix <- function(object) {
    requireNamespace("SummarizedExperiment", quietly = TRUE)
    SummarizedExperiment::assay(object)
}



# `assertive.properties::DIM()`.
.dim <- function(x) {
    dim <- dim(x)
    if (is.null(dim)) {
        length(x)
    } else {
        dim
    }
}



# @seealso `assertive.properties:::get_metric()`.
.getMetric <- function(metric) {
    switch(
        EXPR = metric,
        length = isOfLength,
        elements = hasElements,
        stop("The metric", metric, "is not valid.", domain = NA)
    )
}



# `assertive.properties:::n_elements()`.
.nElements <- function(x) {
    if (is.recursive(x)) {
        sum(vapply(x, .nElements, integer(1L)))
    }
    else {
        as.integer(prod(.dim(x)))
    }
}



# `assertive.base:::to_names()`.
.toNames <- function(x) {
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



.tolerance <- 100L * .Machine[["double.eps"]]



# @seealso `assertive.base::use_first()`.
.useFirst <- function(
    x,
    indexer = c("[[", "["),
    .xname = getNameInParent(x)
) {
    length <- length(x)
    if (length == 0L) {
        stop(sprintf("%s has length 0.", .xname))
    }
    if (length == 1L) {
        return(x)
    }
    indexer <- match.fun(match.arg(indexer))
    x1 <- indexer(x, 1L)
    warning(sprintf(
        "Only the first value of %s (= %s) will be used.",
        .xname, as.character(x1)
    ), call. = FALSE)
    x1
}