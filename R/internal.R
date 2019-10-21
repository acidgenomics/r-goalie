#' Assert check
#'
#' @name check
#' @keywords internal
#'
#' @return
#' `TRUE` on success;
#' `FALSE` on failure, with cause set.
NULL



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



## @seealso `syntactic::capitalize()`.
## Updated 2019-07-15.
.capitalize <- function(x) {
    n <- length(x)
    if (identical(n, 0L)) {
        return(x)
    }
    nas <- is.na(x)
    idxs <- which(nas)
    if (identical(length(idxs), n)) {
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
    assert(requireNamespace("SummarizedExperiment", quietly = TRUE))
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



## Updated 2019-08-10.
.hasCause <- function(x) {
    cause <- cause(x)
    if (
        length(cause) != 1L &&
        !identical(length(x), length(cause))
    ) {
        FALSE
    } else {
        TRUE
    }
}



#' Is an object from a class?
#'
#' Alternate version of [`is()`][methods::is] that supports multiple checks in a
#' single call.
#'
#' If a function named `is.class` exists, call `is.class(x)`.
#' If not, call `is(x, class)`.
#'
#' Used internally by [isAll()][] and [isAny()][] checks.
#'
#' @note Updated 2019-10-04.
#' @noRd
#'
#' @inheritParams acidroxygen::params
#'
#' @seealso
#' - `assertive.base::is2()`.
#' - `methods::is()`.
#'
#' @return `logical`.
#'
#' @examples
#' ## TRUE ====
#' .is2(seq_len(5L), class = "numeric")
#'
#' ## FALSE ====
#' .is2(seq_len(5L), class = "character")
#' .is2(matrix(seq_len(5L)), class = "character")
#' .is2(seq_len(5L), class = c("character", "list", "numeric"))
#' .is2(mean, class = c("function", "data.frame"))
.is2 <- function(x, class, .xname = getNameInParent(x)) {
    if (!is.character(class) || identical(length(class), 0L)) {
        stop("'class' must be non-empty character.")
    }
    if (length(class) > 1L) {
        ok <- bapply(X = class, FUN = function(cl) .is2(x, cl, ""))
        return(setCause(
            x = ok,
            false = sprintf("%s is not '%s'", .typeDescription(x), class)
        ))
    }
    ## Attempt to use `is.character(x)` first.
    ## Otherwise, fall back to `is(x, "character)`.
    ## Alternatively, can take a look at `inherits()` but it doesn't always
    ## return the same TRUE/FALSE as an `is()` call.
    ok <- tryCatch(
        expr = {
            isClass <- match.fun(paste0("is.", class))
            isClass(x)
        },
        error = function(e) {
            is(x, class)
        }
    )
    if (!isTRUE(ok)) {
        return(false(
            "'%s' is not of class '%s'; it has %s.",
            .xname, class, .typeDescription(x)
        ))
    }
    TRUE
}



## @seealso `assertive.base::strip_attributes().
## Updated 2019-07-15.
.stripAttributes <- function(x) {
    attributes(x) <- NULL
    x
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
## Updated 2019-08-10.
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
