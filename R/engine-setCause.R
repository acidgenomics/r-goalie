#' Return `FALSE` vector with causes of failure
#'
#' Sets the `cause` [attribute][base::attributes] of an object and returns that
#' object.
#'
#' @export
#' @keywords internal
#' @note Updated 2019-08-08.
#'
#' @inheritParams acidroxygen::params
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
#' setCause(x = FALSE, false = "test")
setCause <- function(
    x,
    false = "false",
    missing = "missing"
) {
    assert(is.logical(x))
    ## Early return without cause if TRUE.
    ## Consider wrapping in `unname()` call here.
    if (!anyNA(x) && all(x, na.rm = TRUE)) {
        return(x)
    }
    isNA <- is.na(x)
    length <- length(x)
    cause <- character(length = length)
    if (identical(length(missing), 1L)) {
        cause[isNA] <- missing
    } else {
        ## nocov start
        missing <- rep_len(missing, length)
        cause[isNA] <- missing[isNA]
        ## nocov end
    }
    ## Define the FALSE index.
    index <- !(x | isNA)
    if (identical(length(false), 1L)) {
        cause[index] <- false
    } else {
        false <- rep_len(false, length)
        cause[index] <- false[index]
    }
    names(cause) <- names(x)
    cause(x) <- cause
    x
}
