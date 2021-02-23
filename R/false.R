#' Set a false goalie check with cause attribute
#'
#' @export
#' @note Updated 2021-02-23.
#'
#' @param ... Elements to pass to internal [`sprintf()`][base::sprintf] call.
#'
#' @return `goalie`.
#'
#' @examples
#' x <- false("'%s' is invalid.", "xxx")
#' print(x)
#' print(cause(x))
#'
#' ## falseFromVector ====
#' x <- goalie(
#'     object = c("aaa" = FALSE, "bbb" = TRUE, "ccc" = FALSE),
#'     cause = c("xxx", NA, "yyy")
#' )
#' print(x)
#' print(cause(x))
#' xx <- falseFromVector(x)
#' print(xx)
#' print(cause(xx))
false <- function(...) {
    stopifnot(isTRUE(nargs() > 0L))
    goalie(object = FALSE, cause = sprintf(...))
}



## NOTE Subset operation on goalie currently coerces back to logical.
#' @rdname false
#' @export
## Updated 2021-02-23.
`falseFromVector,goalie` <-  # nolint
    function(x) {
        cause <- cause(x)[which(x == FALSE)]
        stopifnot(!is.null(names(cause)))
        x <- mapply(
            name = names(cause),
            value = cause,
            FUN = function(name, value) {
                paste0(name, ": ", value)
            },
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
        )
        x <- unlist(x)
        x <- paste(x, collapse = "; ")
        false(x)
    }



#' @rdname false
#' @export
setMethod(
    f = "falseFromVector",
    signature = signature("goalie"),
    definition = `falseFromVector,goalie`
)
