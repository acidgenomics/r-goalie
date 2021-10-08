#' Is the input scalar?
#'
#' Scalar represents a length of 1.
#'
#' @name check-scalar-isScalar
#' @note Updated 2021-10-07.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `help(topic = "scalar-type-predicates", package = "rlang")`.
#' - `assertive.properties::is_scalar()`.
#' - `rlang::is_scalar_atomic()`.
#' - `rlang::is_scalar_character()`.
#' - `rlang::is_scalar_double()`.
#' - `rlang::is_scalar_integer()`.
#' - `rlang::is_scalar_integerish()`.
#' - `rlang::is_scalar_list()`.
#' - `rlang::is_scalar_logical()`
#' - `rlang::is_scalar_vector()`.
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
    if (is.null(x)) {
        ifelse(
            test = isTRUE(nullOK),
            yes = {
                return(TRUE)
            },
            no = {
                return(false("{.var %s} is {.val %s}.", .xname, "NULL"))
            }
        )
    }
    ok <- identical(length(x), 1L)
    if (!isTRUE(ok)) {
        return(false("{.var %s} doesn't have a length of 1.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarAtomic <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.atomic(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not atomic.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarCharacter <- function(
    x,
    nullOK = TRUE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.character(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not character.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarDouble <- function(
    x,
    nullOK = TRUE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.double(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not double.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarInteger <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.integer(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not integer.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarIntegerish <- function(
    x,
    nullOK = TRUE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- isIntegerish(x, .xname = .xname)
    if (!isTRUE(ok)) {
        return(ok)
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarList <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.list(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not list.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarLogical <- function(
    x,
    nullOK = TRUE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.logical(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not logical.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarNumeric <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.numeric(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not numeric.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isScalarVector <- function(
    x,
    nullOK = FALSE,
    .xname = getNameInParent(x)
) {
    ok <- isScalar(x, nullOK = nullOK, .xname = .xname)
    if (is.null(x) && isTRUE(ok)) {
        return(TRUE)
    } else if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- is.vector(x)
    if (!isTRUE(ok)) {
        return(false("{.var %s} is not vector.", .xname))
    }
    TRUE
}



#' @rdname check-scalar-isScalar
#' @export
isNonScalar <- function(
    x,
    .xname = getNameInParent(x)
) {
    ok <- isFALSE(isScalar(x))
    if (!isTRUE(ok)) {
        return(false("{.var %s} is scalar (has a length of 1).", .xname))
    }
    TRUE
}
