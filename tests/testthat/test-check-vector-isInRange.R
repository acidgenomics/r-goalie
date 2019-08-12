context("check : vector : isInRange")

lower <- 0L
upper <- 1L

test_that("TRUE", {
    x <- c(0L, 0.5, 1L)

    ok <- isInRange(x, lower = lower, upper = upper)
    expect_true(all(ok))
    ok <- allAreInRange(x, lower = lower, upper = upper)
    expect_true(ok)

    ## `isInRange()` defaults to closed range.
    ok <- isInClosedRange(x, lower = lower, upper = upper)
    expect_true(all(ok))
    ok <- allAreInClosedRange(x, lower = lower, upper = upper)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(2L, 3L)

    ok <- isInRange(x, lower = lower, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreInRange(x, lower = lower, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(ok)

    ok <- isInClosedRange(x, lower = lower, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreInClosedRange(x, lower = lower, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))
})



context("check : vector : isInOpenRange")

test_that("TRUE", {
    x <- c(0.25, 0.5, 0.75)

    ok <- isInOpenRange(x, lower = lower, upper = upper)
    expect_true(all(ok))

    ok <- allAreInOpenRange(x, lower = lower, upper = upper)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(0L, 1L)

    ok <- isInOpenRange(x, lower = lower, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreInOpenRange(x, lower = lower, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isInLeftOpenRange")

test_that("TRUE", {
    x <- c(0.5, 0.75, 1L)

    ok <- isInLeftOpenRange(x, lower = lower, upper = upper)
    expect_true(all(ok))

    ok <- allAreInLeftOpenRange(x, lower = lower, upper = upper)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(-1L, -0.5, 0L)

    ok <- isInLeftOpenRange(x, lower = lower)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreInLeftOpenRange(x, lower = lower)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isInRightOpenRange")

test_that("TRUE", {
    x <- c(0L, 0.25, 0.5)

    ok <- isInRightOpenRange(x, lower = lower, upper = upper)
    expect_true(any(ok))

    ok <- allAreInRightOpenRange(x, lower = lower, upper = upper)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(1L, 2L, 3L)

    ok <- isInRightOpenRange(x, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreInRightOpenRange(x, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isNegative")

test_that("TRUE", {
    x <- c(-2L, -1L)

    ok <- isNegative(x)
    expect_true(all(ok))

    ok <- allAreNegative(x)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(0L, 1L)

    ok <- isNegative(x)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreNegative(x)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isPositive")

test_that("TRUE", {
    x <- c(1L, 2L)

    ok <- isPositive(x)
    expect_true(all(ok))

    ok <- allArePositive(x)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(-1L, 0L)

    ok <- isPositive(x)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allArePositive(x)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isNonNegative")

test_that("TRUE", {
    x <- c(0L, 1L)

    ok <- isNonNegative(x)
    expect_true(all(ok))

    ok <- allAreNonNegative(x)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(-2L, -1L)

    ok <- isNonNegative(x)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreNonNegative(x)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isNonPositive")

test_that("TRUE", {
    x <- c(-1L, 0L)

    ok <- isNonPositive(x)
    expect_true(all(ok))

    ok <- allAreNonPositive(x)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(1L, 2L)

    ok <- isNonPositive(x)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreNonPositive(x)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isPercentage")

test_that("TRUE", {
    x <- c(0L, 25L, 50L, 100L)

    ok <- isPercentage(x)
    expect_true(all(ok))

    ok <- allArePercentage(x)
    expect_true(ok)
})

## This may be too strict. Consider allowing 110%.
test_that("FALSE", {
    x <- c(-10L, 110L)

    ok <- isPercentage(x)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allArePositive(x)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : vector : isProportion")

test_that("TRUE", {
    x <- c(0L, 0.01, 0.1, 1L)

    ok <- isProportion(x)
    expect_true(all(ok))

    ok <- allAreProportion(x)
    expect_true(ok)
})

## This may be strict. Consider allowing > 1.
test_that("FALSE", {
    x <- c(-0.1, 1.1)

    ok <- isProportion(x)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreProportion(x)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("check : scalar : allArePercentage")

test_that("FALSE", {
    ok <- allArePercentage(c(100L, 200L))
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_match(cause(ok), "too high")
})
