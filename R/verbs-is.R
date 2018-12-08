# help(topic = "scalar-type-predicates", package = "rlang")
#
# Don't reexport `assertive::is_subset()` without hardening first. It doesn't
# currently error on NULL values as expected.
#
# Don't reexport `assertive::is_scalar()`. Use the rlang approach instead which
# requires a more specific declaration of the expected scalar type.



# isAny ========================================================================
#' @describeIn checkAnyClass Less strict variant that calls `assertive::is2()`
#'   internally, and allows for inherited classes to pass.
#' @export
isAny <- function(x, classes) {
    any(is2(x, class = classes))
}

#' @describeIn checkAnyClass snake alias.
#' @usage NULL
#' @export
is_any <- isAny



# isEmptyFile ==================================================================
#' @importFrom assertive.files is_empty_file
#' @export
assertive.files::is_empty_file

#' @describeIn reexports `is_empty_file` camel alias.
#' @usage NULL
#' @export
isEmptyFile <- is_empty_file



# isExisting ===================================================================
#' @importFrom assertive.code is_existing
#' @export
assertive.code::is_existing

#' @rdname reexports
#' @usage NULL
#' @export
isExisting <- is_existing



# isHexColor ===================================================================
#' @importFrom assertive.data is_hex_color
#' @export
assertive.data::is_hex_color

#' @rdname reexports
#' @usage NULL
#' @export
isHexColor <- is_hex_color



# isIntegerish =================================================================
#' @importFrom rlang is_integerish
#' @export
rlang::is_integerish

#' @rdname reexports
#' @usage NULL
#' @export
isIntegerish <- is_integerish



# isNegative ===================================================================
#' @importFrom assertive.numbers is_negative
#' @export
assertive.numbers::is_negative

#' @rdname reexports
#' @usage NULL
#' @export
isNegative <- is_negative



# isNonEmptyFile ===============================================================
#' @importFrom assertive.files is_non_empty_file
#' @export
assertive.files::is_non_empty_file

#' @rdname reexports
#' @usage NULL
#' @export
isNonEmptyFile <- is_non_empty_file



# isNotNA ======================================================================
#' @importFrom assertive.base is_not_na
#' @export
assertive.base::is_not_na

#' @rdname reexports
#' @usage NULL
#' @export
isNotNA <- is_not_na



# isPositive ===================================================================
#' @importFrom assertive.numbers is_positive
#' @export
assertive.numbers::is_positive

#' @rdname reexports
#' @usage NULL
#' @export
isPositive <- is_positive



# isScalarDouble ===============================================================
#' @importFrom rlang is_scalar_double
#' @export
rlang::is_scalar_double

#' @rdname reexports
#' @usage NULL
#' @export
isScalarDouble <- is_scalar_double



# isScalarIntegerish ===========================================================
#' @importFrom rlang is_scalar_integerish
#' @export
rlang::is_scalar_integerish

#' @rdname reexports
#' @usage NULL
#' @export
isScalarIntegerish <- is_scalar_integerish



# isString =====================================================================
#' @importFrom rlang is_string
#' @export
rlang::is_string

#' @rdname reexports
#' @usage NULL
#' @export
isString <- is_string
