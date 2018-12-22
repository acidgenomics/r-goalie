#' Is the input in range?
#'
#' @name isInRange
#' @inherit params
#'
#' @section Intervals:
#'
#' - Closed: Includes all its limit points, and is denoted with square brackets.
#'   For example, `[0,1]` means greater than or equal to 0 and less than or
#'   equal to 1.
#' - Open: Does not include its endpoints, and is indicated with parentheses.
#'   For example, `(0,1)` means greater than 0 and less than 1.
#'
#' @param closed `logical(2)`.
#'   Should the lower (1) and upper (2) bounaries be closed?
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
#' ## Pass ====
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
#' ## Fail ====
#' isInRange(c(2, 3), lower = 0, upper = 1)
#' isInClosedRange(c(2, 3), lower = 0, upper = 1)
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



# vector =======================================================================
#' @describeIn isInRange Vectorized.
#' @export
isInRange <- function(
    x,
    lower = -Inf,
    upper = Inf,
    closed = c(TRUE, TRUE),
    .xname = getNameInParent(x)
) {
    assert(
        is.numeric(lower) && !is.na(lower),
        is.numeric(upper) && !is.na(upper),
        is.logical(closed) && length(closed) == 2L
    )

    ok <- is.numeric(x) && !is.na(x)
    if (!isTRUE(ok)) {
        return(false("%s is not (non-NA) numeric.", .xname))
    }

    tooLow <- (if (closed[[1L]]) `<` else `<=`)(x, lower)
    tooHigh <- (if (closed[[2L]]) `>` else `>=`)(x, upper)

    ok <- rep.int(TRUE, length(x))
    ok[tooLow] <- FALSE
    ok[tooHigh] <- FALSE
    names(ok) <- x

    setCause(ok, false = ifelse(tooLow, "too low", "too high"))
}



#' @describeIn isInRange Vectorized.
#' @export
isInClosedRange <- function(
    x,
    lower = -Inf,
    upper = Inf,
    .xname = getNameInParent(x)
) {
    isInRange(
        x = x,
        lower = lower,
        upper = upper,
        closed = c(TRUE, TRUE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isInOpenRange <- function(
    x,
    lower = -Inf,
    upper = Inf,
    .xname = getNameInParent(x)
) {
    isInRange(
        x = x,
        lower = lower,
        upper = upper,
        closed = c(FALSE, FALSE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isInLeftOpenRange <- function(
    x,
    lower = -Inf,
    upper = Inf,
    .xname = getNameInParent(x)
) {
    isInRange(
        x = x,
        lower = lower,
        upper = upper,
        closed = c(FALSE, TRUE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isInRightOpenRange <- function(
    x,
    lower = -Inf,
    upper = Inf,
    .xname = getNameInParent(x)
) {
    isInRange(
        x = x,
        lower = lower,
        upper = upper,
        closed = c(TRUE, FALSE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isNegative <- function(x, .xname = getNameInParent(x)) {
    isInRange(
        x = x,
        lower = -Inf,
        upper = 0L,
        closed = c(TRUE, FALSE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isPositive <- function(x, .xname = getNameInParent(x)) {
    isInRange(
        x = x,
        lower = 0L,
        upper = Inf,
        closed = c(FALSE, TRUE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isNonNegative <- function(x, .xname = getNameInParent(x)) {
    isInRange(
        x = x,
        lower = 0L,
        upper = Inf,
        closed = c(TRUE, TRUE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isNonPositive <- function(x, .xname = getNameInParent(x)) {
    isInRange(
        x = x,
        lower = -Inf,
        upper = 0L,
        closed = c(TRUE, TRUE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isPercentage <- function(x, .xname = getNameInParent(x)) {
    isInRange(
        x = x,
        lower = 0L,
        upper = 100L,
        closed = c(TRUE, TRUE),
        .xname = .xname
    )
}



#' @describeIn isInRange Vectorized.
#' @export
isProportion <- function(x, .xname = getNameInParent(x)) {
    isInRange(
        x = x,
        lower = 0L,
        upper = 1L,
        closed = c(TRUE, TRUE),
        .xname = .xname
    )
}



# scalar =======================================================================
#' @describeIn isInRange Scalar.
#' @export
allAreInRange <- function() {
    ok <- isInRange(
        x = x, lower = lower, upper = upper, closed = closed, .xname = .xname
    )
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreInRange) <- formals(isInRange)



#' @describeIn isInRange Scalar.
#' @export
allAreInClosedRange <- function() {
    ok <- isInClosedRange(x = x, lower = lower, upper = upper, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreInClosedRange) <- formals(isInClosedRange)



#' @describeIn isInRange Scalar.
#' @export
allAreInOpenRange <- function() {
    ok <- isInOpenRange(x = x, lower = lower, upper = upper, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreInOpenRange) <- formals(isInOpenRange)



#' @describeIn isInRange Scalar.
#' @export
allAreInLeftOpenRange <- function() {
    ok <- isInLeftOpenRange(
        x = x, lower = lower, upper = upper, .xname = .xname
    )
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreInLeftOpenRange) <- formals(isInLeftOpenRange)



#' @describeIn isInRange Scalar.
#' @export
allAreInRightOpenRange <- function() {
    ok <- isInRightOpenRange(
        x = x, lower = lower, upper = upper, .xname = .xname
    )
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreInRightOpenRange) <- formals(isInRightOpenRange)



#' @describeIn isInRange Scalar.
#' @export
allAreNegative <- function() {
    ok <- isNegative(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreNegative) <- formals(isNegative)



#' @describeIn isInRange Scalar.
#' @export
allArePositive <- function() {
    ok <- isPositive(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allArePositive) <- formals(isPositive)



#' @describeIn isInRange Scalar.
#' @export
allAreNonNegative <- function() {
    ok <- isNonNegative(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreNonNegative) <- formals(isNonNegative)



#' @describeIn isInRange Scalar.
#' @export
allAreNonPositive <- function() {
    ok <- isNonPositive(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreNonPositive) <- formals(isNonPositive)



#' @describeIn isInRange Scalar.
#' @export
allArePercentage <- function() {
    ok <- isPercentage(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allArePercentage) <- formals(isPercentage)



#' @describeIn isInRange Scalar.
#' @export
allAreProportion <- function() {
    ok <- isProportion(x = x, .xname = .xname)
    if (!all(ok)) return(falseFromVector(ok))
    TRUE
}

formals(allAreProportion) <- formals(isProportion)