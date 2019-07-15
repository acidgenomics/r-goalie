# @seealso `base::stopifnot()`.
.Dparse <-  # nolint
    function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) {
            paste(ch[[1L]], "....")  # nocov
        } else {
            ch
        }
    }



# @seealso `base::stopifnot()`.
.abbrev <- function(ae, n = 3L) {
    paste(c(.head(ae, n), if (length(ae) > n) "...."), collapse = "\n  ")
}



# @seealso `syntactic::capitalize()`.
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
        stop("The metric `", metric, "` is not valid.", domain = NA)
    )
}



# @seealso `base::stopifnot()`.
.head <- function(x, n = 6L) {
    x[seq_len(if (n < 0L) max(length(x) + n, 0L) else min(n, length(x)))]
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
