#' Does the input have names?
#'
#' @name check-scalar-hasNames
#' @note Updated 2019-08-10.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
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
hasNames <- function(x) {
    names <- tryCatch(
        expr = names(x),
        error = function(e) {
            e
        }
    )
    if (is(names, "error")) {
        false(
            "{.fun %s} command on {.var %s} failed.",
            "names", toCauseName(x)
        )
    } else if (is.null(names)) {
        false(
            "The names of {.var %s} are {.val %s}.",
            toCauseName(x), "NULL"
        )
    } else if (!any(nzchar(names))) {
        false("The names of {.var %s} are all empty.", toCauseName(x))
    } else {
        TRUE
    }
}
