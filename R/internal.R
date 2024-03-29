#' Assert check
#'
#' @name check
#' @keywords internal
#'
#' @return
#' `TRUE` on success;
#' `FALSE` on failure, with cause set.
NULL



#' Capitalize
#'
#' @note Updated 2019-10-30.
#' @noRd
.capitalize <- function(x) {
    vapply(
        X = as.character(x),
        FUN = function(x) {
            if (is.na(x)) {
                return(x)
            }
            first <- toupper(substring(x, first = 1L, last = 1L))
            tail <- substring(x, first = 2L)
            paste0(first, tail)
        },
        FUN.VALUE = character(1L),
        USE.NAMES = FALSE
    )
}



#' Coerce a SummarizedExperiment to a matrix
#'
#' @details Using primary assay here.
#'
#' @note Updated 2021-01-04.
#' @noRd
.coerceSummarizedExperimentToMatrix <- function(object) {
    requireNamespaces("SummarizedExperiment")
    SummarizedExperiment::assay(object)
}



#' Deparse
#'
#' @note Updated 2021-02-23.
#' @noRd
.deparse <-
    function(call, cutoff = 60L) {
        ch <- deparse(call, width.cutoff = cutoff)
        if (length(ch) > 1L) {
            ch <- paste(ch[[1L]], "....")
        }
        ch
    }



#' Get dimensions
#'
#' @note Updated 2021-02-23.
#' @noRd
#'
#' @seealso `assertive.properties::DIM()`.
.dim <- function(x) {
    dim <- dim(x)
    if (is.null(dim)) {
        dim <- length(x)
    }
    dim
}



#' Head of vector
#'
#' @note Updated 2023-10-06.
#' @noRd
#'
#' @seealso
#' - `utils::head`.
.head <- function(x) {
    x[seq(from = 1L, to = min(length(x), 6L))]
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
#' @note Updated 2023-10-02.
#' @noRd
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
.is2 <- function(x, class) {
    if (!is.character(class) || identical(length(class), 0L)) {
        stop("'class' must be non-empty character.")
    }
    if (length(class) > 1L) {
        ok <- bapply(
            X = class,
            FUN = function(cl) {
                .is2(x, cl)
            },
            USE.NAMES = FALSE
        )
        return(setCause(
            object = ok,
            false = sprintf("%s is not {.var %s}", .typeDescription(x), class)
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
            "{.var %s} is not of class {.var %s}; it has %s.",
            .toName(x), class, .typeDescription(x)
        ))
    }
    TRUE
}



#' Expression deparsing
#'
#' Turn unevaluated expressions into character strings.
#'
#' [safeDeparse()] is modified version of [`deparse()`][base::deparse] that
#' always returns `character(1)`.
#'
#' @note Updated 2020-01-04.
#' @noRd
#'
#' @param expr `expression`.
#' Any R expression.
#'
#' @param ... Passed to [`deparse()`][base::deparse].
#'
#' @seealso
#' - `assertive.base::safe_deparse()`.
#' - `deparse()`.
#'
#' @return `character(1)`.
#'
#' @examples
#' .safeDeparse(is.character("a"))
.safeDeparse <- function(expr, ...) {
    paste0(deparse(expr, width.cutoff = 500L, ...), collapse = "")
}



#' Return the object as a name for use in cause attribute
#'
#' @note Updated 2023-10-02.
#' @noRd
.toName <- function(x) {
    if (is.null(x)) {
        return("NULL")
    }
    cls <- as.character(class(x))[[1L]]
    if (!is.atomic(x)) {
        return(cls)
    }
    if (identical(length(x), 1L)) {
        return(as.character(x))
    }
    cls
}



#' Get the type description
#'
#' @note Updated 2021-02-23.
#' @noRd
#'
#' @seealso `assertive.base:::type_description()`.
#'
#' @details
#' .typeDescription("xxx")
.typeDescription <- function(x) {
    if (is.array(x)) {
        x <- sprintf(
            fmt = "class '%s %s'",
            class(x[FALSE]), # nolint
            toString(class(x))
        )
    } else if (is.function(x)) {
        x <- sprintf(
            fmt = "class '%s %s'",
            typeof(x), toString(class(x))
        )
    } else if (isS4(x)) {
        x <- sprintf("S4 class {.var %s}", toString(class(x)))
    } else {
        x <- sprintf("class {.var %s}", toString(class(x)))
    }
    x
}
