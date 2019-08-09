context("isEqual")

test_that("TRUE", {
    x <- c(1L, 1)  # nolint
    y <- 1L

    ok <- isEqualTo(x = x, y = y)
    expect_true(all(ok))

    ok <- allAreEqualTo(x = x, y = y)
    expect_true(ok)
})

test_that("FALSE", {
    x <- seq_len(2L)
    y <- 0L

    ok <- isEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_identical(
        nocause(ok),
        c(`1` = FALSE, `2` = FALSE)
    )
    expected <- noquote(c(
        "not equal to 0; abs diff = 1",
        "not equal to 0; abs diff = 2"
    ))
    names(expected) <- x
    expect_identical(cause(ok), expected)

    ok <- allAreEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("isNotEqualTo")

test_that("TRUE", {
    x <- seq_len(2L)
    y <- 0L

    ok <- isNotEqualTo(x = x, y = y)
    expect_true(all(ok))

    ok <- allAreNotEqualTo(x = x, y = y)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(1L, 1L)
    y <- 1L

    ok <- isNotEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreNotEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("isGreaterThan")

test_that("TRUE", {
    x <- seq_len(2L)
    y <- 0L

    ok <- isGreaterThan(x = x, y = y)
    expect_true(all(ok))

    ok <- allAreGreaterThan(x = x, y = y)
    expect_true(ok)
})

test_that("FALSE", {
    x <- seq_len(2L)
    y <- 3L

    ok <- isGreaterThan(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreGreaterThan(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("isGreaterThanOrEqualTo")

test_that("TRUE", {
    x <- seq_len(2L)
    y <- 1L

    ok <- isGreaterThanOrEqualTo(x = x, y = y)
    expect_true(all(ok))

    ok <- allAreGreaterThanOrEqualTo(x = x, y = y)
    expect_true(ok)
})

test_that("FALSE", {
    x <- seq_len(2L)
    y <- 3L

    ok <- isGreaterThanOrEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreGreaterThanOrEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("isLessThan")

test_that("TRUE", {
    x <- seq_len(2L)
    y <- 3L

    ok <- isLessThan(x = x, y = y)
    expect_true(all(ok))

    ok <- allAreLessThan(x = x, y = y)
    expect_true(ok)
})

test_that("FALSE", {
    x <- seq_len(2L)
    y <- 0L

    ok <- isLessThan(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreLessThan(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})



context("isLessThanOrEqualTo")

test_that("TRUE", {
    x <- seq_len(2L)
    y <- 3L

    ok <- isLessThanOrEqualTo(x = x, y = y)
    expect_true(all(ok))

    ok <- allAreLessThanOrEqualTo(x = x, y = y)
    expect_true(ok)
})

test_that("FALSE", {
    x <- seq_len(2L)
    y <- 0L

    ok <- isLessThanOrEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(any(ok))

    ok <- allAreLessThanOrEqualTo(x = x, y = y)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})
