context("isInRange")

lower <- 0L
upper <- 1L

test_that("TRUE", {
    x <- c(0L, 0.5, 1L)

    ok <- isInRange(x, lower = lower, upper = upper)
    expect_true(all(ok))
    ok <- allAreInRange(x, lower = lower, upper = upper)
    expect_true(ok)

    # `isInRange()` defaults to closed range.
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

    ok <- isInClosedRange(x, lower = lower, upper = upper)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))
})



context("isInOpenRange")

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
})



context("isInLeftOpenRange")

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
})



context("isInRightOpenRange")

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
})



context("isNegative")

test_that("TRUE", {
    x <- c(-2L, -1L)
    ok <- isNegative(x)
    expect_true(all(ok))
    ok <- allAreNegative(x)
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isNegative(1L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
})



context("isPositive")

test_that("TRUE", {
    x <- c(1L, 2L)
    ok <- isPositive(x)
    expect_true(all(ok))
    ok <- allArePositive(x)
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isPositive(-1L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
})



context("isNonNegative")

test_that("TRUE", {
    x <- c(0L, 1L)
    ok <- isNonNegative(x)
    expect_true(all(ok))
    ok <- allAreNonNegative(x)
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isNonNegative(-1L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
})



context("isNonPositive")

test_that("TRUE", {
    x <- c(-1L, 0L)
    ok <- isNonPositive(x)
    expect_true(all(ok))
    ok <- allAreNonPositive(x)
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isNonPositive(1L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
})



context("isPercentage")

test_that("TRUE", {
    x <- c(0L, 25L, 50L, 100L)
    ok <- isPercentage(x)
    expect_true(all(ok))
    ok <- allArePercentage(x)
    expect_true(ok)
})

# This may be too strict. Consider allowing 110%.
test_that("FALSE", {
    ok <- isPercentage(110L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
})



context("isProportion")

test_that("TRUE", {
    x <- c(0L, 0.01, 0.1, 1L)
    ok <- isProportion(x)
    expect_true(all(ok))
    ok <- allAreProportion(x)
    expect_true(ok)
})

# This may be strict. Consider allowing > 1.
test_that("FALSE", {
    ok <- isProportion(1.1)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
})
