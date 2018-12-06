#' Validate Classes
#'
#' Validity check capable of validating multiple slots in a single call.
#'
#' To be used inside S4 `methods::setValidity()` call or with
#' `assertthat::validate_that()`. Particularly useful for checking multiple
#' slotted objects inside `metadata()`.
#'
#' @export
#'
#' @inheritParams params
#' @param expected `list`. Named list of expected classes per slot.
#' @param subset `boolean`. Only check a subset of slots in the object.
#'
#' @seealso `assertthat::validate_that()`.
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
    assert_is_list(expected)
    assert_has_names(expected)
    assert_is_a_bool(subset)
    if (isTRUE(subset)) {
        assert_is_subset(names(expected), names(object))
    } else {
        assert_are_set_equal(names(expected), names(object))
    }
    valid <- mapply(
        slot = names(expected),
        classes = expected,
        MoreArgs = list(object = object),
        FUN = function(slot, classes, object) {
            intersect <- intersect(
                x = classes,
                y = class(object[[slot]])
            )
            if (!has_length(intersect)) {
                FALSE
            } else {
                TRUE
            }
        },
        SIMPLIFY = TRUE,
        USE.NAMES = TRUE
    )
    assert_is_logical(valid)
    ifelse(
        test = all(valid),
        yes = TRUE,
        no = paste(
            "Class checks failed:",
            printString(names(valid)[!valid]),
            sep = "\n"
        )
    )
}
