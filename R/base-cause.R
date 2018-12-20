#' Get or set the `cause` attribute
#'
#' Gets or sets the `cause` (of failure) [attribute][base::attributes] of a
#' variable.
#'
#' @export
#' @inheritParams params
#'
#' @return `character(1)`. Cause attribute.
#'
#' @seealso
#' - `assertive.base::cause()`.
#' - `attributes()`.
#' - `attr()`.
#'
#' @examples
#' ## Scalar cause.
#' x <- isFlag("xxx")
#' cause(x)
#'
#' ## Vector cause.
#' x <- isInRange(c(1L, 2L), lower = 3L)
#' cause(x)
cause <- function(x) {
    attr(x, "cause")
}



#' @rdname cause
#' @export
`cause<-` <- function(x, value) {
    if (
        length(value) != 1L &&
        length(value) != length(x)
    ) {
        stop(sprintf(
            paste(
                "The length of value should be 1",
                "or the length of x (%d), but is %d."
            ),
            length(x), length(value)
        ))
    }
    attr(x, "cause") <- noquote(as.character(value))
    class(x) <- c("goalie", "logical")
    x
}



#' Return `FALSE` vector with causes of failure
#'
#' Sets the `cause` [attribute][base::attributes] of an object and returns that
#' object.
#'
#' @export
#' @inheritParams params
#'
#' @param false `character`.
#'   A character vector to set the cause to, when `x` is `FALSE`.
#' @param missing `character`.
#'   A character vector to set the cause to, when `x` is `NA`.
#'
#' @return `goalie`/`logical`.
#'
#' @seealso
#' - `cause()`.
#' - `assertive.base::set_cause()`.
#' - `stats::setNames()`.
#'
#' @examples
#' setCause(FALSE, false = "test")
setCause <- function(
    x,
    false,
    missing = "missing"
) {
    assert(is.logical(x))
    # Early return without cause if TRUE.
    if (!anyNA(x) && all(x, na.rm = TRUE)) {
        # Ensure names are removed.
        names(x) <- NULL
        return(x)
    }
    isNA <- is.na(x)
    length <- length(x)
    cause <- character(length)
    if (length(missing) == 1L) {
        cause[isNA] <- missing
    }
    else {
        missing <- rep_len(missing, length)
        cause[isNA] <- missing[isNA]
    }
    # Define the FALSE index.
    index <- !(x | isNA)
    if (length(false) == 1L) {
        cause[index] <- false
    }
    else {
        false <- rep_len(false, length)
        cause[index] <- false[index]
    }
    cause(x) <- cause
    x
}



#' Return `FALSE` scalar with cause of failure
#'
#' Always returns the value `FALSE`, with a `cause`
#' [attribute][base::attributes].
#'
#' @export
#'
#' @param ... Passed to [gettextf()][base::gettextf] to create a [cause] of
#'   failure message.
#'
#' @return `goalie`/`logical(1L)`.
#'
#' @seealso `assertive.base::false()`.
false <- function(...) {
    msg <- if (nargs() > 0L) {
        sprintf(...)
    } else {
        ""
    }
    x <- FALSE
    cause(x) <- msg[[1L]]
    x
}
