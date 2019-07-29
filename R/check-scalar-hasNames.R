#' Does the input have names?
#'
#' @name check-scalar-hasNames
#' @inherit params
#' @note Updated 2019-07-29.
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



## Added a `tryCatch()` call here to handle `names()` error on invalid objects
## that extend SummarizedExperiment (e.g. bcbioRNASeq bcb_invalid.rda example).

#' @rdname check-scalar-hasNames
#' @export
## Updated 2019-07-29.
hasNames <- function(x, .xname = getNameInParent(x)) {
    names <- tryCatch(
        expr = names(x),
        error = function(e) e
    )
    if (is(names, "error")) {
        false("`names()` command on %s failed.", .xname)  # nocov
    } else if (is.null(names)) {
        false("The names of %s are NULL.", .xname)
    } else if (!any(nzchar(names))) {
        false("The names of %s are all empty.", .xname)
    } else {
        TRUE
    }
}
