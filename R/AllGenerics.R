#' @rdname cause
#' @export
setGeneric(
    name = "cause",
    def = function(object) {
        standardGeneric("cause")
    }
)



#' @rdname show
#' @name show
#' @importFrom methods show
#' @usage show(object)
#' @export
NULL



#' @rdname setCause
#' @export
setGeneric(
    name = "setCause",
    def = function(x, ...) {
        standardGeneric("setCause")
    }
)
