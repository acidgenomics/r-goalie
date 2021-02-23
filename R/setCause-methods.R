#' Return `FALSE` vector with causes of failure
#'
#' Sets the `cause` [attribute][base::attributes] of an object and returns that
#' object.
#'
#' @name setCause
#' @note Updated 2021-02-23.
#'
#' @inheritParams AcidRoxygen::params
#' @param false `character`.
#'   A character vector to set the cause to, when `x` is `FALSE`.
#' @param missing `character`.
#'   A character vector to set the cause to, when `x` is `NA`.
#'
#' @return `goalie`.
#'
#' @seealso
#' - `cause()`.
#' - `assertive.base::set_cause()`.
#' - `stats::setNames()`.
#'
#' @examples
#' x <- setCause(x = FALSE, false = "test")
#' print(x)
#' cause(x)
NULL



## Updated 2021-02-23.
`setCause,logical` <- function(
    x,
    false = "false",
    missing = "missing"
) {
    ## Early return without cause if TRUE.
    ## Consider wrapping in `unname()` call here.
    if (!anyNA(x) && all(x, na.rm = TRUE)) {
        return(x)
    }
    isNA <- is.na(x)
    length <- length(x)
    cause <- rep(x = NA_character_, times = length)
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
    goalie(x, cause = cause)
}



#' @rdname setCause
#' @export
setMethod(
    f = "setCause",
    signature = signature("logical"),
    definition = `setCause,logical`
)
