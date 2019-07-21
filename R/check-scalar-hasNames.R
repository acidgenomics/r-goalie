#' Does the input have names?
#'
#' @name check-scalar-hasNames
#' @inherit params
#'
#' @seealso `assertive.properties::has_names()`.
#'
#' @examples
#' ## TRUE ====
#' hasNames(datasets::mtcars)
#'
#' ## FALSE ====
#' hasNames(matrix())
#' hasNames(data.frame())
NULL



#' @rdname check-scalar-hasNames
#' @export
## Updated 2019-07-15.
hasNames <- function(x, .xname = getNameInParent(x)) {
    namesx <- names(x)
    if (is.null(namesx)) {
        return(false("The names of %s are NULL.", .xname))
    }
    if (!any(nzchar(namesx))) {
        return(false("The names of %s are all empty.", .xname))
    }
    TRUE
}
