# TODO Pick this or Hadley's assertthat approach using fail.



#' Get or set the `cause` attribute
#'
#' Gets or sets the `cause` (of failure) [attribute][base::attributes] of a
#' variable.
#'
#' @export
#' @return `character(1)`. Cause attribute.
#'
#' @seealso
#' - `assertive.base::cause()`.
#' - `attributes()`.
#' - `attr()`.
#'
#' @examples
#' ## FIXME
#'
#' ## ## Scalar case
#' ## yn <- assertive.base::is_identical_to_true(FALSE)
#' ## cause(yn)
#'
#' ## ## Vector case
#' ## yn <- assertive.base::is_true(c(TRUE, FALSE, NA))
#' ## cause(yn)
cause <- function(x) {
    y <- attr(x, "cause")
    if (is.null(y)) {
        return(noquote(character(length(x))))
    }
    y
}



#' @rdname cause
#' @export
`cause<-` <- function(x, value) {
    if (length(value) != 1 && length(value) != length(x)) {
        stop(sprintf(
            paste(
                "The length of value should be 1",
                "or the length of x (%d), but is %d."
            ),
            length(x), length(value)
        ))
    }
    attr(x, "cause") <- noquote(as.character(value))
    x
}
