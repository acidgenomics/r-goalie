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
#' data(rse)
#' validateClasses(
#'     object = S4Vectors::metadata(rse),
#'     expected = list(
#'         version = c("package_version", "numeric_version"),
#'         date = "Date",
#'         interestingGroups = "character"
#'     )
#' )
validateClasses <- function(
    object,
    expected,
    subset = FALSE
) {
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
            printString(names(valid)[!valid]),
            sep = "\n"
        )
    }
}
