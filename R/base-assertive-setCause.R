# TODO Set either `vector_with_cause` or `scalar_with_cause`.

#' Set a Cause and Return the Input
#'
#' Sets the cause attribute of an object and returns that object.
#'
#' @export
#'
#' @param x Variable.
#' @param falseValue `character`.
#'   A character vector to set the cause to, when `x` is `FALSE`.
#' @param missingValue `character`.
#'   A character vector to set the cause to, when `x` is `NA`.
#'
#' @seealso
#' - `assertive.base::set_cause`.
#' - `cause`.
#' - `setNames`.
#'
#' @examples
#' setCause(FALSE, falseValue = "test")
setCause <- function(x, falseValue, missingValue = "missing") {
    if (!anyNA(x) && all(x, na.rm = TRUE)) {
        return(x)
    }
    isNA <- is.na(x)
    length <- length(x)
    causeValue <- character(length)
    if (length(missingValue) == 1L) {
        causeValue[isNA] <- missingValue
    }
    else {
        missingValue <- rep_len(missingValue, length)
        causeValue[isNA] <- missingValue[isNA]
    }
    falseIndex <- !(x | isNA)
    if (length(falseValue) == 1L) {
        causeValue[falseIndex] <- falseValue
    }
    else {
        falseValue <- rep_len(falseValue, length)
        causeValue[falseIndex] <- falseValue[falseIndex]
    }
    cause(x) <- causeValue
    class(x) <- c("vector_with_cause", "logical")
    x
}
