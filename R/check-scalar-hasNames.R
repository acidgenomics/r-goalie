#' Does the input have names?
#'
#' @export
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
