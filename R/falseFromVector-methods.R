#' Set a scalar false goalie check with cause attribute
#'
#' @name falseFromVector
#' @note Updated 2021-02-23.
#'
#' @return `goalie`.
#'
#' @examples
#' x <- goalie(
#'     object = c("aaa" = FALSE, "bbb" = TRUE, "ccc" = FALSE),
#'     cause = c("xxx", NA, "yyy")
#' )
#' print(x)
#' print(cause(x))
#' xx <- falseFromVector(x)
#' print(xx)
#' print(cause(xx))
NULL



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
