#' Is the input in range?
#'
#' @name check-vector-isInRange
#' @note Updated 2022-12-14.
#'
#' @section Intervals:
#'
#' - Closed: Includes all its limit points, and is denoted with square brackets.
#' For example, `[0,1]` means greater than or equal to 0 and less than or
#' equal to 1.
#' - Open: Does not include its endpoints, and is indicated with parentheses.
#' For example, `(0,1)` means greater than 0 and less than 1.
#'
#' @inherit check
#' @inheritParams AcidRoxygen::params
#'
#' @seealso
#' - `assertive.numbers::is_in_range()`.
#' - `assertive.numbers::is_in_closed_range()`.
#' - `assertive.numbers::is_in_open_range()`.
#' - `assertive.numbers::is_in_left_open_range()`.
#' - `assertive.numbers::is_in_right_open_range()`.
#' - `assertive.numbers::is_negative()`.
#' - `assertive.numbers::is_positive()`.
#' - `assertive.numbers::is_non_negative()`.
#' - `assertive.numbers::is_non_positive()`.
#' - `assertive.numbers::is_percentage()`.
#' - `assertive.numbers::is_proportion()`.
#'
#' @examples
#' ## TRUE ====
#' isInRange(0, lower = 0, upper = 1)
#' isInRange(1, lower = 0, upper = 1)
#' isInClosedRange(1, lower = 0, upper = 1)
#'
#' isInOpenRange(0.5, lower = 0, upper = 1)
#' isInLeftOpenRange(1, lower = 0, upper = 1)
#' isInRightOpenRange(0, lower = 0, upper = 1)
#'
#' isNegative(c(-2, -1))
#' isPositive(c(1, 2))
#'
#' isNonNegative(c(0, 1))
#' isNonPositive(c(-1, 0))
#'
#' isPercentage(c(0, 25, 50, 100))
#' isProportion(c(0, 0.01, 0.1, 1))
#'
#' ## FALSE ====
#' isInRange(c(-1, 2), lower = 0, upper = 1)
#' isInClosedRange(c(0, 1), lower = 0, upper = 1)
#'
#' isInOpenRange(c(1, 2), lower = 0, upper = 1)
#' isInLeftOpenRange(0, lower = 0)
#' isInRightOpenRange(1, upper = 1)
#'
#' isPositive(-1)
#' isNegative(1)
#'
#' isPercentage(110)
#' isProportion(1.1)
NULL



## Vector ======================================================================

#' @describeIn check-vector-isInRange Vectorized.
#' @export
isInRange <-
    function(x,
             lower = -Inf,
             upper = Inf,
             closed = c(TRUE, TRUE),
             .xname = getNameInParent(x)) {
        if (is(x, "Rle")) {
            assert(requireNamespace("S4Vectors", quietly = TRUE))
            x <- S4Vectors::decode(x)
        }
        assert(
            is.numeric(x),
            is.vector(x),
            !anyNA(x),
            is.numeric(lower) && !is.na(lower),
            is.numeric(upper) && !is.na(upper),
            is.logical(closed) && identical(length(closed), 2L)
        )
        tooLow <- (if (closed[[1L]]) `<` else `<=`)(x, lower)
        tooHigh <- (if (closed[[2L]]) `>` else `>=`)(x, upper)
        ok <- rep.int(TRUE, length(x))
        ok[tooLow] <- FALSE
        ok[tooHigh] <- FALSE
        names(ok) <- .toNames(x)
        setCause(ok, false = ifelse(tooLow, "too low", "too high"))
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isInClosedRange <-
    function(x,
             lower = -Inf,
             upper = Inf,
             .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = lower,
            upper = upper,
            closed = c(TRUE, TRUE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isInOpenRange <-
    function(x,
             lower = -Inf,
             upper = Inf,
             .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = lower,
            upper = upper,
            closed = c(FALSE, FALSE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isInLeftOpenRange <-
    function(x,
             lower = -Inf,
             upper = Inf,
             .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = lower,
            upper = upper,
            closed = c(FALSE, TRUE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isInRightOpenRange <-
    function(x,
             lower = -Inf,
             upper = Inf,
             .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = lower,
            upper = upper,
            closed = c(TRUE, FALSE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isNegative <-
    function(x, .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = -Inf,
            upper = 0L,
            closed = c(TRUE, FALSE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isPositive <-
    function(x, .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = 0L,
            upper = Inf,
            closed = c(FALSE, TRUE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isNonNegative <-
    function(x, .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = 0L,
            upper = Inf,
            closed = c(TRUE, TRUE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isNonPositive <-
    function(x, .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = -Inf,
            upper = 0L,
            closed = c(TRUE, TRUE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isPercentage <-
    function(x, .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = 0L,
            upper = 100L,
            closed = c(TRUE, TRUE),
            .xname = .xname
        )
    }



#' @describeIn check-vector-isInRange Vectorized.
#' @export
isProportion <-
    function(x, .xname = getNameInParent(x)) {
        isInRange(
            x = x,
            lower = 0L,
            upper = 1L,
            closed = c(TRUE, TRUE),
            .xname = .xname
        )
    }



## Scalar ======================================================================

#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreInRange <-
    function(x,
             lower,
             upper,
             closed,
             .xname) {
        ok <- isInRange(
            x = x,
            lower = lower,
            upper = upper,
            closed = closed,
            .xname = .xname
        )
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }

formals(allAreInRange) <- formals(isInRange)



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreInClosedRange <-
    function(x,
             lower,
             upper,
             .xname) {
        ok <- isInClosedRange(
            x = x,
            lower = lower,
            upper = upper,
            .xname = .xname
        )
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }

formals(allAreInClosedRange) <- formals(isInClosedRange)



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreInOpenRange <-
    function(x,
             lower,
             upper,
             .xname) {
        ok <- isInOpenRange(
            x = x,
            lower = lower,
            upper = upper,
            .xname = .xname
        )
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }

formals(allAreInOpenRange) <- formals(isInOpenRange)



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreInLeftOpenRange <-
    function(x,
             lower,
             upper,
             .xname) {
        ok <- isInLeftOpenRange(
            x = x,
            lower = lower,
            upper = upper,
            .xname = .xname
        )
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }

formals(allAreInLeftOpenRange) <- formals(isInLeftOpenRange)



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreInRightOpenRange <-
    function(x,
             lower,
             upper,
             .xname) {
        ok <- isInRightOpenRange(
            x = x,
            lower = lower,
            upper = upper,
            .xname = .xname
        )
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }

formals(allAreInRightOpenRange) <- formals(isInRightOpenRange)



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreNegative <-
    function(x, .xname = getNameInParent(x)) {
        ok <- isNegative(x = x, .xname = .xname)
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }



#' @describeIn check-vector-isInRange Scalar.
#' @export
allArePositive <-
    function(x, .xname = getNameInParent(x)) {
        ok <- isPositive(x = x, .xname = .xname)
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreNonNegative <-
    function(x, .xname = getNameInParent(x)) {
        ok <- isNonNegative(x = x, .xname = .xname)
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreNonPositive <-
    function(x, .xname = getNameInParent(x)) {
        ok <- isNonPositive(x = x, .xname = .xname)
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }



#' @describeIn check-vector-isInRange Scalar.
#' @export
allArePercentage <-
    function(x, .xname = getNameInParent(x)) {
        ok <- isPercentage(x = x, .xname = .xname)
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }



#' @describeIn check-vector-isInRange Scalar.
#' @export
allAreProportion <-
    function(x, .xname = getNameInParent(x)) {
        ok <- isProportion(x = x, .xname = .xname)
        if (!all(ok)) {
            return(falseFromVector(ok))
        }
        TRUE
    }
