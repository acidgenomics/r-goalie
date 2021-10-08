#' Is the R session running as vanilla?
#'
#' @name check-scalar-isVanilla
#' @note Updated 2020-08-11.
#'
#' @inherit check return
#'
#' @examples
#' isVanilla()
NULL



#' @rdname check-scalar-isVanilla
#' @export
isVanilla <-
    function() {
        ok <- isSubset("--vanilla", commandArgs())
        if (!isTRUE(ok)) {
            return(false(
                "R session is not running with {.arg %s} flag.",
                "--vanilla"
            ))
        }
        TRUE
    }
