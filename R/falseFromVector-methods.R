#' Set a scalar false goalie check with cause attribute
#'
#' @name falseFromVector
#' @note Updated 2023-10-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return `goalie`.
#'
#' @examples
#' x <- goalie(
#'     object = c(FALSE, TRUE, FALSE),
#'     cause = c("xxx", NA, "yyy")
#' )
#' print(x)
#' print(cause(x))
#' xx <- falseFromVector(x)
#' print(xx)
#' print(cause(xx))
NULL



## Updated 2023-10-02.
`falseFromVector,goalie` <- # nolint
    function(object) {
        if (isFALSE(object)) {
            return(object)
        }
        idx <- which(object == FALSE)
        cause <- cause(object)[idx]
        object <- object[idx]
        x <- Map(
            f = function(pos, value) {
                paste0(pos, ": ", value)
            },
            pos = idx,
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
