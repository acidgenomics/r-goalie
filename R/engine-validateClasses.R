## FIXME Inform the user about `updateObject()` function.



#' Validate expected classes
#'
#' Validity check capable of validating multiple slots in a single call.
#'
#' To be used inside S4 [`setValidity()`][methods::setValidity()] call or with
#' [validate()]. Particularly useful for checking multiple slotted objects
#' inside [`metadata()`][S4Vectors::metadata].
#'
#' @name engine-validateClasses
#' @note Updated 2021-01-04.
#'
#' @inheritParams AcidRoxygen::params
#' @param expected `list`.
#'   Named list of expected classes per slot.
#' @param subset `logical(1)`.
#'   Only check a subset of slots in the object.
#'
#' @seealso
#' - `validate()`.
#' - `methods::setValidity()`.
#' - `methods::validObject()`.
#'
#' @return `logical(1)` (`TRUE`) on sucess or `character(1)` containing
#'   informative message on failure.
#'
#' @examples
#' validateClasses(
#'     object = list(
#'         a = character(),
#'         b = integer(),
#'         c = factor()
#'     ),
#'     expected = list(
#'         a = "character",
#'         b = "integer",
#'         c = "factor"
#'     )
#' )
NULL



#' @rdname engine-validateClasses
#' @export
validateClasses <- function(object, expected, subset = FALSE) {
    assert(
        is(expected, "list"),
        isFlag(subset)
    )
    if (isTRUE(subset)) {
        assert(isSubset(names(expected), names(object)))
    } else {
        assert(areSetEqual(names(expected), names(object)))
    }
    valid <- mapply(
        slot = names(expected),
        classes = expected,
        MoreArgs = list(object = object),
        FUN = function(slot, classes, object) {
            intersect <- intersect(classes, class(object[[slot]]))
            if (identical(length(intersect), 0L)) {
                FALSE
            } else {
                TRUE
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
    if (!all(valid)) {
        return(paste(
            "Class checks failed:",
            capture.output(print(names(valid)[!valid])),
            sep = "\n"
        ))
    }
    TRUE
}
