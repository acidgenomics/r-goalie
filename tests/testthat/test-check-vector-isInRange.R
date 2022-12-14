lower <- 0L
upper <- 1L

test_that("isInClosedRange : TRUE", {
    x <- c(0L, 0.5, 1L)
    ok <- isInClosedRange(x, lower = lower, upper = upper)
    expect_true(all(ok))
    ok <- allAreInClosedRange(x, lower = lower, upper = upper)
    expect_true(ok)
    ok <- isInRange(x, lower = lower, upper = upper)
    expect_true(all(ok))
    ok <- allAreInRange(x, lower = lower, upper = upper)
    expect_true(ok)
    ok <- isInRange(x = S4Vectors::Rle(x), lower = lower, upper = upper)
    expect_true(all(ok))
})

test_that("isInClosedRange : FALSE", {
    x <- c(2L, 3L)
    ok <- isInRange(x, lower = lower, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreInRange(x, lower = lower, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    ok <- isInClosedRange(x, lower = lower, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreInClosedRange(x, lower = lower, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
})

test_that("isInOpenRange : TRUE", {
    x <- c(0.25, 0.5, 0.75)
    ok <- isInOpenRange(x, lower = lower, upper = upper)
    expect_true(all(ok))
    ok <- allAreInOpenRange(x, lower = lower, upper = upper)
    expect_true(ok)
})

test_that("isInOpenRange : FALSE", {
    x <- c(0L, 1L)
    ok <- isInOpenRange(x, lower = lower, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreInOpenRange(x, lower = lower, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isInLeftOpenRange : TRUE", {
    x <- c(0.5, 0.75, 1L)
    ok <- isInLeftOpenRange(x, lower = lower, upper = upper)
    expect_true(all(ok))
    ok <- allAreInLeftOpenRange(x, lower = lower, upper = upper)
    expect_true(ok)
})

test_that("isInLeftOpenRange : FALSE", {
    x <- c(-1L, -0.5, 0L)
    ok <- isInLeftOpenRange(x, lower = lower)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreInLeftOpenRange(x, lower = lower)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("IsInRightOpenRange : TRUE", {
    x <- c(0L, 0.25, 0.5)
    ok <- isInRightOpenRange(x, lower = lower, upper = upper)
    expect_true(any(ok))
    ok <- allAreInRightOpenRange(x, lower = lower, upper = upper)
    expect_true(ok)
})

test_that("IsInRightOpenRange : FALSE", {
    x <- c(1L, 2L, 3L)
    ok <- isInRightOpenRange(x, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreInRightOpenRange(x, upper = upper)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isNegative : TRUE", {
    x <- c(-2L, -1L)
    ok <- isNegative(x)
    expect_true(all(ok))
    ok <- allAreNegative(x)
    expect_true(ok)
})

test_that("isNegative : FALSE", {
    x <- c(0L, 1L)
    ok <- isNegative(x)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreNegative(x)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isPositive : TRUE", {
    x <- c(1L, 2L)
    ok <- isPositive(x)
    expect_true(all(ok))
    ok <- allArePositive(x)
    expect_true(ok)
})

test_that("isPositive : FALSE", {
    x <- c(-1L, 0L)
    ok <- isPositive(x)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allArePositive(x)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isNonNegative : TRUE", {
    x <- c(0L, 1L)
    ok <- isNonNegative(x)
    expect_true(all(ok))
    ok <- allAreNonNegative(x)
    expect_true(ok)
})

test_that("isNonNegative : FALSE", {
    x <- c(-2L, -1L)
    ok <- isNonNegative(x)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreNonNegative(x)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isNonPositive : TRUE", {
    x <- c(-1L, 0L)
    ok <- isNonPositive(x)
    expect_true(all(ok))
    ok <- allAreNonPositive(x)
    expect_true(ok)
})

test_that("isNonPositive : FALSE", {
    x <- c(1L, 2L)
    ok <- isNonPositive(x)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreNonPositive(x)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isPercentage : TRUE", {
    x <- c(0L, 25L, 50L, 100L)
    ok <- isPercentage(x)
    expect_true(all(ok))
    ok <- allArePercentage(x)
    expect_true(ok)
})

## NOTE This may be too strict. Consider allowing above 100%.
test_that("isPercentage : FALSE", {
    x <- c(-10L, 110L)
    ok <- isPercentage(x)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allArePercentage(c(100L, 200L))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_match(cause(ok), "too high")
})

test_that("isProportion : TRUE", {
    x <- c(0L, 0.01, 0.1, 1L)
    ok <- isProportion(x)
    expect_true(all(ok))
    ok <- allAreProportion(x)
    expect_true(ok)
})

## NOTE This may be strict. Consider allowing > 1.
test_that("isProportion : FALSE", {
    x <- c(-0.1, 1.1)
    ok <- isProportion(x)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreProportion(x)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})
