## FIXME CAUSE MUST BE A STRING (SCALAR).



#' Get or set the `cause` attribute
#'
#' Gets or sets the [cause] (of failure) [attribute][base::attributes] of a
#' variable.
#'
#' @name engine-cause
#' @note Updated 2021-01-04.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `character(1)`.
#' Cause attribute.
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
NULL



## FIXME NEED TO REWORK USING S4.

#' @rdname engine-cause
#' @export
## Updated 2019-07-15.
cause <- function(x) {
    attr(x, "cause")
}



#' @rdname engine-cause
#' @export
## Updated 2019-08-08.
`cause<-` <-  # nolint
    function(x, value) {
        stopifnot(is.character(value))
        if (length(value) != 1L && length(value) != length(x)) {
            stop(sprintf(
                fmt = paste0(
                    "The length of 'value' should be 1 ",
                    "or the length of 'x' (%d), but is %d."
                ),
                length(x),
                length(value)
            ))
        } else if (length(x) > 1L && !is.character(names(value))) {
            stop("'value' containing multiple elements must be named.")
        }
        ## FIXME RETHINK THIS?
        ## FIXME DONT USE NOQUOTE HERE?
        ## > attr(x, "cause") <- noquote(value)
        ## > class(x) <- c("goalie", "logical")
        new(Class = "goalie", x)
    }



## FIXME REWORK...

#' @rdname engine-cause
#' @export
## Updated 2019-07-15.
nocause <- function(x) {
    attr(x, "cause") <- NULL
    class(x) <- "logical"
    x
}
