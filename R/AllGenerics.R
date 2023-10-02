#' @rdname cause
#' @export
setGeneric(
    name = "cause",
    def = function(object, ...) {
        standardGeneric("cause")
    }
)

#' @rdname falseFromVector
#' @export
setGeneric(
    name = "falseFromVector",
    def = function(object, ...) {
        standardGeneric("falseFromVector")
    }
)

#' @rdname nocause
#' @export
setGeneric(
    name = "nocause",
    def = function(object, ...) {
        standardGeneric("nocause")
    }
)

#' @rdname setCause
#' @export
setGeneric(
    name = "setCause",
    def = function(object, ...) {
        standardGeneric("setCause")
    }
)
