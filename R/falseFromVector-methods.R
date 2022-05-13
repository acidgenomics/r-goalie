#' Set a scalar false goalie check with cause attribute
#'
#' @name falseFromVector
#' @note Updated 2022-05-13.
#'
#' @inheritParams AcidRoxygen::params
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



## Updated 2022-05-13.
`falseFromVector,goalie` <- # nolint
    function(object) {
        cause <- cause(object)
        if (isFALSE(object) && is.null(names(cause))) {
            return(object)
        }
        cause <- cause[which(object == FALSE)]
        x <- Map(
            f = function(name, value) {
                paste0(name, ": ", value)
            },
            name = names(cause),
            value = cause
        )
        x <- unlist(x)
        x <- toString(x, width = 500L)
        ## Need to ensure "%" is encoded as "%%" before handing off to
        ## cause attribute setter, which calls `sprintf` internally.
        x <- gsub(pattern = "%", replacement = "%%", x = x)
        false(x)
    }



#' @rdname falseFromVector
#' @export
setMethod(
    f = "falseFromVector",
    signature = signature(object = "goalie"),
    definition = `falseFromVector,goalie`
)
