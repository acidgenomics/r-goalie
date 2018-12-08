#' Validate Classes
#'
#' Validity check capable of validating multiple slots in a single call.
#'
#' To be used inside S4 `methods::setValidity()` call or with `validate()`.
#' Particularly useful for checking multiple slotted objects inside
#' `metadata()`.
#'
#' @export
#'
#' @inheritParams params
#' @param expected `list`. Named list of expected classes per slot.
#' @param subset `boolean`. Only check a subset of slots in the object.
#'
#' @seealso
#' - `validate()`.
#' - `methods::setValidity()`.
#' - `methods::validObject()`.
#'
#' @return `boolean` (TRUE) on sucess or `string` containing informative
#'   message on failure.
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
validateClasses <- function(object, expected, subset = FALSE) {
    assert(
        is(expected, "list"),
        hasNames(expected),
        testFlag(subset)
    )
    if (isTRUE(subset)) {
        assertNames(names(object), must.include = names(expected))
    } else {
        assertNames(names(object), permutation.of = names(expected))
    }
    valid <- mapply(
        slot = names(expected),
        classes = expected,
        MoreArgs = list(object = object),
        FUN = function(slot, classes, object) {
            intersect <- intersect(classes, class(object[[slot]]))
            if (length(intersect) == 0L) {
                FALSE
            } else {
                TRUE
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
    assertLogical(valid)
    if (all(valid)) {
        TRUE
    } else {
        paste(
            "Class checks failed:",
            # See `basejump::printString()` for current method.
            capture.output(print(names(valid)[!valid])),
            sep = "\n"
        )
    }
}
