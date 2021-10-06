## FIXME Add support for `nullOK = TRUE`.


#' Is the input scalar?
#'
#' Scalar represents a length of 1.
#'
#' @name check-scalar-isScalar
#' @note Updated 2021-10-06.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `help(topic = "scalar-type-predicates", package = "rlang")`.
#' - `assertive.properties::is_scalar()`.
#' - `rlang::is_scalar_list()`.
#' - `rlang::is_scalar_atomic()`.
#' - `rlang::is_scalar_vector()`.
#' - `rlang::is_scalar_integer()`.
#' - `rlang::is_scalar_integerish()`.
#' - `rlang::is_scalar_double()`.
#' - `rlang::is_scalar_character()`.
#' - `rlang::is_scalar_logical()`.
#'
#' @examples
#' ## TRUE ====
#' isScalar("a")
#' isScalarInteger(1L)
#' isScalarIntegerish(1)
#'
#' ## FALSE ====
#' isScalar(NULL)
#' isScalar(c("a", "b"))
NULL



#' @rdname check-scalar-isScalar
#' @export
isScalar <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ## Conditionally allow NULL.
    if (isTRUE(nullOK) && is.null(x)) {
        return(TRUE)
    }
    ok <- identical(length(x), 1L)
    if (!isTRUE(ok)) {
        return(false("'%s' does not have a length of 1.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarList <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.list(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not list.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarAtomic <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.atomic(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not atomic.", .xname))  # nocov
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarVector <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.vector(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not vector.", .xname))  # nocov
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarNumeric <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.numeric(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not numeric.", .xname))  # nocov
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarInteger <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.integer(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not integer.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarIntegerish <- function(x) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    if (!isTRUE(ok <- isIntegerish(x))) return(ok)
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarDouble <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.double(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not double.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarCharacter <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.character(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not character.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarLogical <- function(x, .xname = getNameInParent(x)) {
    if (!isTRUE(ok <- isScalar(x))) return(ok)
    ok <- is.logical(x)
    if (!isTRUE(ok)) {
        return(false("'%s' is not logical.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isNonScalar <- function(x, .xname = getNameInParent(x)) {
    ok <- as.logical(!isScalar(x))
    if (!isTRUE(ok)) {
        return(false("'%s' is scalar (has a length of 1).", .xname))
    }
    TRUE
}
