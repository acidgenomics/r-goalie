context("check : vector : isInRange")

test_that("TRUE", {
    expect_true(isInRange(0L, lower = 0L, upper = 1L))
    expect_true(isInRange(1L, lower = 0L, upper = 1L))
    expect_true(isInClosedRange(1L, lower = 0L, upper = 1L))

    expect_true(isInOpenRange(0.5, lower = 0L, upper = 1L))
    expect_true(isInLeftOpenRange(1L, lower = 0L, upper = 1L))
    expect_true(isInRightOpenRange(0L, lower = 0L, upper = 1L))

    expect_true(all(isNegative(c(-2L, -1L))))
    expect_true(all(isPositive(c(1L, 2L))))
    expect_true(all(isNonNegative(c(0L, 1L))))
    expect_true(all(isNonPositive(c(-1L, 0L))))

    expect_true(all(isPercentage(c(0L, 25L, 50L, 100L))))
    expect_true(all(isProportion(c(0L, 0.01, 0.1, 1L))))
})

test_that("FALSE", {
    ok <- isInRange(c(2L, 3L), lower = 0L, upper = 1L)
    expect_s3_class(ok, "goalie")
    expect_false(all(as.logical(ok)), c(FALSE, FALSE))

    ok <- isInClosedRange(c(2L, 3L), lower = 0L, upper = 1L)
    expect_s3_class(ok, "goalie")
    expect_false(all(as.logical(ok)), c(FALSE, FALSE))

    ok <- isInOpenRange(c(1L, 2L), lower = 0L, upper = 1L)
    expect_s3_class(ok, "goalie")
    expect_false(all(as.logical(ok)), c(FALSE, FALSE))

    expect_false(isInLeftOpenRange(0L, lower = 0L))
    expect_false(isInRightOpenRange(1L, upper = 1L))

    expect_false(isPositive(-1L))
    expect_false(isNegative(1L))

    expect_false(isPercentage(110L))
    expect_false(isProportion(1.1))
})
