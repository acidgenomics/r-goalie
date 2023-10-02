#' Is the input scalar?
#'
#' Scalar represents a length of 1.
#'
#' @name check-scalar-isScalar
#' @note Updated 2021-10-08.
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
isScalar <-
    function(x, nullOk = FALSE) {
        if (is.null(x)) {
            ifelse(
                test = isTRUE(nullOk),
                yes = {
                    return(TRUE)
                },
                no = {
                    return(false(
                        "{.var %s} is {.val %s}.",
                        .toName(x), "NULL"
                    ))
                }
            )
        }
        ok <- identical(length(x), 1L)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} doesn't have a length of 1.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarAtomic <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.atomic(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not atomic.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarCharacter <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.character(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not character.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarDouble <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.double(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not double.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarInteger <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.integer(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not integer.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarIntegerish <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- isIntegerish(x)
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarList <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.list(x)
        if (!isTRUE(ok)) {
            return(false("{.var %s} is not list.", .toName(x)))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarLogical <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.logical(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not logical.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarNumeric <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.numeric(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not numeric.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isScalarVector <-
    function(x, nullOk = FALSE) {
        ok <- isScalar(x, nullOk = nullOk)
        if (is.null(x) && isTRUE(ok)) {
            return(TRUE)
        } else if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- is.vector(x)
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is not vector.",
                .toName(x)
            ))
        }
        TRUE
    }



#' @rdname check-scalar-isScalar
#' @export
isNonScalar <-
    function(x) {
        ok <- isFALSE(isScalar(x))
        if (!isTRUE(ok)) {
            return(false(
                "{.var %s} is scalar (has a length of 1).",
                .toName(x)
            ))
        }
        TRUE
    }
