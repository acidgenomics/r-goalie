#' @rdname cause
#' @export
setGeneric(
    name = "cause",
    def = function(object, ...) {
        standardGeneric("cause")
    }
)



## FIXME RENAME TO OBJECT.

#' @rdname false
#' @export
setGeneric(
    name = "falseFromVector",
    def = function(x, ...) {
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




#' @rdname show
#' @name show
#' @importFrom methods show
#' @usage show(object)
#' @export
NULL



## FIXME RENAME TO OBJECT.

#' @rdname setCause
#' @export
setGeneric(
    name = "setCause",
    def = function(x, ...) {
        standardGeneric("setCause")
    }
)
