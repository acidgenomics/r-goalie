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
#' x <- setCause(
#'     object = c(TRUE, TRUE, FALSE, FALSE, NA, NA),
#'     false = "foo",
#'     missing = "bar"
#' )
#' print(x)
#' cause(x)
NULL



## Updated 2021-02-23.
`setCause,logical` <- function(
    object,
    false = "false",
    missing = "missing"
) {
    ## Early return without cause if TRUE.
    ## Consider wrapping in `unname()` call here.
    if (!anyNA(object) && all(object, na.rm = TRUE)) {
        return(object)
    }
    isNA <- is.na(object)
    length <- length(object)
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
    index <- !(object | isNA)
    if (identical(length(false), 1L)) {
        cause[index] <- false
    } else {
        false <- rep_len(false, length)
        cause[index] <- false[index]
    }
    goalie(object = object, cause = cause)
}



#' @rdname setCause
#' @export
setMethod(
    f = "setCause",
    signature = signature("logical"),
    definition = `setCause,logical`
)



## Updated 2021-02-23.
`setCause,goalie` <-  # nolint
    function(object, ...) {
        names <- names(object)
        object <- as.logical(object)
        names(object) <- names
        setCause(object = object, ...)
    }



#' @rdname setCause
#' @export
setMethod(
    f = "setCause",
    signature = signature("goalie"),
    definition = `setCause,goalie`
)
