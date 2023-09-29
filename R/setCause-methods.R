#' Return `FALSE` vector with causes of failure
#'
#' Sets the `cause` [attribute][base::attributes] of an object and returns that
#' object.
#'
#' @name setCause
#' @note Updated 2021-02-23.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param false `character`.
#' A character vector to set the cause to, when `x` is `FALSE`.
#'
#' @param missing `character`.
#' A character vector to set the cause to, when `x` is `NA`.
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
`setCause,logical` <- # nolint
    function(object,
             false = "false",
             missing = "missing") {
        if (!anyNA(object) && all(object, na.rm = TRUE)) {
            object <- unname(object)
            return(object)
        }
        isNA <- is.na(object)
        length <- length(object)
        cause <- rep(x = NA_character_, times = length)
        if (identical(length(missing), 1L)) {
            cause[isNA] <- missing
        } else {
            missing <- rep_len(missing, length)
            cause[isNA] <- missing[isNA]
        }
        index <- !(object | isNA)
        if (identical(length(false), 1L)) {
            cause[index] <- false
        } else {
            false <- rep_len(false, length)
            cause[index] <- false[index]
        }
        names(cause) <- names(object)
        object <- unname(object)
        goalie(object = object, cause = cause)
    }



## Updated 2021-02-23.
`setCause,goalie` <- # nolint
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
    signature = signature(object = "goalie"),
    definition = `setCause,goalie`
)

#' @rdname setCause
#' @export
setMethod(
    f = "setCause",
    signature = signature(object = "logical"),
    definition = `setCause,logical`
)
