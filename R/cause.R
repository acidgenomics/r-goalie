#' Get or set the `cause` attribute
#'
#' Gets or sets the [cause] (of failure) [attribute][base::attributes] of a
#' variable.
#'
#' @export
#' @inheritParams params
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
