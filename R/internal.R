## @seealso `base::stopifnot()`.
## Updated 2019-07-15.
.Dparse <-  # nolint
    function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) {
            paste(ch[[1L]], "....")  # nocov
        } else {
            ch
        }
    }



## Updated 2019-07-15.
.assertHasCause <- function(x) {
    cause <- cause(x)
    if (
        length(cause) != 1L &&
        !identical(length(x), length(cause))
    ) {
        stop("cause error.")
    }
    TRUE
}



## @seealso `syntactic::capitalize()`.
## Updated 2019-07-15.
.capitalize <- function(x) {
    n <- length(x)
    if (n == 0L) {
        return(x)
    }
    nas <- is.na(x)
    idxs <- which(nas)
    if (length(idxs) == n) {
        return(x)  # nocov
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



## Using primary assay here.
.coerceSummarizedExperimentToMatrix <- function(object) {
    requireNamespace("SummarizedExperiment", quietly = TRUE)
    SummarizedExperiment::assay(object)
}



## @seealso `assertive.properties::DIM()`.
## Updated 2019-07-15.
.dim <- function(x) {
    dim <- dim(x)
    if (is.null(dim)) {
        length(x)
    } else {
        dim
    }
}



## @seealso `assertive.properties:::get_metric()`.
## Updated 2019-07-15.
.getMetric <- function(metric) {
    switch(
        EXPR = metric,
        length = isOfLength,
        elements = hasElements,
        stop("The metric `", metric, "` is not valid.", domain = NA)
    )
}



## @seealso `assertive.properties:::n_elements()`.
## Updated 2019-07-15.
.nElements <- function(x) {
    if (is.recursive(x)) {
        sum(vapply(x, .nElements, integer(1L)))
    }
    else {
        as.integer(prod(.dim(x)))
    }
}



## @seealso `assertive.base::strip_attributes().
## Updated 2019-07-15.
.stripAttributes <- function(x) {
    attributes(x) <- NULL
    x
}



## @seealso `assertive.base:::to_names()`.
## Updated 2019-07-15.
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



## @seealso `assertive.base:::truncate()`.
## Updated 2019-07-15.
.truncate <- function(x, width = getOption("width")) {
    x <- as.character(x)
    ifelse(
        test = nchar(x) > width,
        yes = paste0(substring(x, 1L, width - 3L), "..."),
        no = x
    )
}



## @seealso `assertive.base:::type_description()`.
.typeDescription <- function(x) {
    if (is.array(x)) {
        sprintf(
            fmt = "class '%s %s'",
            class(x[FALSE]),  # nolint
            toString(class(x))
        )
    } else if (is.function(x)) {
        sprintf(
            fmt = "class '%s %s'",
            typeof(x), toString(class(x))
        )
    } else if (isS4(x)) {
        sprintf("S4 class '%s'", toString(class(x)))
    } else {
        sprintf("class '%s'", toString(class(x)))
    }
}
