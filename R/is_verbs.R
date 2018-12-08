# isAny ========================================================================
#' @describeIn checkAnyClass Less strict variant that calls `assertive::is2()`
#'   internally, and allows for inherited classes to pass.
#' @export
isAny <- function(x, classes) {
    any(is2(x, class = classes))
}

#' @rdname checkAnyClass
#' @export
is_any <- isAny



# isScalarDouble ===============================================================
#' @importFrom rlang is_scalar_double
#' @export
rlang::is_scalar_double

#' @rdname reexports
#' @usage NULL
#' @export
isScalarDouble <- is_scalar_double



# isString =====================================================================
#' @importFrom rlang is_string
#' @export
rlang::is_string

#' @rdname reexports
#' @usage NULL
#' @export
isString <- is_string



# isIntegerish =================================================================
#' @importFrom rlang is_integerish
#' @export
rlang::is_integerish

#' @rdname reexports
#' @usage NULL
#' @export
isIntegerish <- is_integerish



# isScalarIntegerish ===========================================================
#' @importFrom rlang is_scalar_integerish
#' @export
rlang::is_scalar_integerish

#' @rdname reexports
#' @usage NULL
#' @export
isScalarIntegerish <- is_scalar_integerish
