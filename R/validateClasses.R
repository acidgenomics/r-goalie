#' Validate expected classes
#'
#' Validity check capable of validating multiple slots in a single call.
#'
#' To be used inside S4 [`setValidity()`][methods::setValidity()] call or with
#' [validate()]. Particularly useful for checking multiple slotted objects
#' inside [`metadata()`][S4Vectors::metadata].
#'
#' @export
#' @note Updated 2022-06-13.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param expected `list`.
#' Named list of expected classes per slot.
#'
#' @param subset `logical(1)`.
#' Only check a subset of slots in the object.
#'
#' @seealso
#' - `validate()`.
#' - `methods::setValidity()`.
#' - `methods::validObject()`.
#'
#' @return `logical(1)` (`TRUE`) on sucess or `character(1)` containing
#' informative message on failure.
#'
#' @examples
#' ## TRUE ====
#' validateClasses(
#'     object = list(
#'         "a" = character(),
#'         "b" = integer(),
#'         "c" = factor()
#'     ),
#'     expected = list(
#'         "a" = "character",
#'         "b" = "integer",
#'         "c" = "factor"
#'     )
#' )
#'
#' ## FALSE ====
#' validateClasses(
#'     object = list(
#'         "a" = character(),
#'         "b" = integer(),
#'         "c" = factor()
#'     ),
#'     expected = list(
#'         "a" = "character",
#'         "b" = "character",
#'         "c" = "character"
#'     )
#' )
validateClasses <-
    function(object, expected, subset = FALSE) {
        assert(
            is(expected, "list"),
            isFlag(subset)
        )
        if (isTRUE(subset)) {
            assert(isSubset(names(expected), names(object)))
        } else {
            assert(areSetEqual(names(expected), names(object)))
        }
        valid <- as.logical(Map(
            f = function(slot, classes, object) {
                isAny(x = object[[slot]], classes = classes)
            },
            slot = names(expected),
            classes = expected,
            MoreArgs = list("object" = object)
        ))
        names(valid) <- names(expected)
        if (all(valid)) {
            return(TRUE)
        }
        paste0(
            "Class checks failed: ",
            toString(names(valid)[!valid], width = 200L), ".\n",
            "If supported, 'updateObject()' ",
            "may help resolve these issues."
        )
    }
