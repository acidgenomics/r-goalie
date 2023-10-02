#' Return `FALSE` vector with causes of failure
#'
#' Sets the `cause` [attribute][base::attributes] of an object and returns that
#' object.
#'
#' @name setCause
#' @note Updated 2023-10-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param false `character`.
#' A character vector to set the cause to, when `x` is `FALSE`.
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
#'     object = c(TRUE, TRUE, FALSE, FALSE),
#'     false = "false"
#' )
#' print(x)
#' print(cause(x))
NULL



## Updated 2023-10-02.
`setCause,logical` <- # nolint
    function(object, false) {
        if (anyNA(object)) {
            stop("Object contains NA.")
        }
        if (all(object)) {
            object <- unname(object)
            return(object)
        }
        ln <- length(object)
        cause <- rep(x = NA_character_, times = ln)
        idx <- !object
        if (identical(length(false), 1L)) {
            cause[idx] <- false
        } else {
            false <- rep_len(false, ln)
            cause[idx] <- false[idx]
        }
        if (is.null(names(object))) {
            names(cause) <- as.character(seq_along(object))
        } else {
            names(cause) <- names(object)
            object <- unname(object)
        }
        goalie(object = object, cause = cause)
    }



## Updated 2023-09-29.
`setCause,goalie` <- # nolint
    function(object, ...) {
        names <- names(cause(object))
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
