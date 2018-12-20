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
