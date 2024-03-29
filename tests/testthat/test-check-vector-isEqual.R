test_that("isEqualTo : TRUE", {
    x <- c(1L, 1) # nolint
    y <- 1L
    ok <- isEqualTo(x = x, y = y)
    expect_true(all(ok))
    ok <- allAreEqualTo(x = x, y = y)
    expect_true(ok)
    ok <- isEqualTo(x = S4Vectors::Rle(x), y = S4Vectors::Rle(y))
    expect_true(all(ok))
})

test_that("isEqualTo : FALSE", {
    x <- seq_len(2L)
    y <- 0L
    ok <- isEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_identical(nocause(ok), rep(FALSE, 2L))
    expect_identical(
        object = cause(ok),
        expected = c(
            "not equal to 0; abs diff = 1",
            "not equal to 0; abs diff = 2"
        )
    )
    ok <- allAreEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isNotEqualTo : TRUE", {
    x <- seq_len(2L)
    y <- 0L
    ok <- isNotEqualTo(x = x, y = y)
    expect_true(all(ok))
    ok <- allAreNotEqualTo(x = x, y = y)
    expect_true(ok)
})

test_that("isNotEqualTo : FALSE", {
    x <- c(1L, 1L)
    y <- 1L
    ok <- isNotEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreNotEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isGreaterThan : TRUE", {
    x <- seq_len(2L)
    y <- 0L
    ok <- isGreaterThan(x = x, y = y)
    expect_true(all(ok))
    ok <- allAreGreaterThan(x = x, y = y)
    expect_true(ok)
})

test_that("isGreaterThan : FALSE", {
    x <- seq_len(2L)
    y <- 3L
    ok <- isGreaterThan(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreGreaterThan(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isGreaterThanOrEqualTo : TRUE", {
    x <- seq_len(2L)
    y <- 1L
    ok <- isGreaterThanOrEqualTo(x = x, y = y)
    expect_true(all(ok))
    ok <- allAreGreaterThanOrEqualTo(x = x, y = y)
    expect_true(ok)
})

test_that("isGreaterThanOrEqualTo : FALSE", {
    x <- seq_len(2L)
    y <- 3L
    ok <- isGreaterThanOrEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreGreaterThanOrEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isLessThan : TRUE", {
    x <- seq_len(2L)
    y <- 3L
    ok <- isLessThan(x = x, y = y)
    expect_true(all(ok))
    ok <- allAreLessThan(x = x, y = y)
    expect_true(ok)
})

test_that("isLessThan : FALSE", {
    x <- seq_len(2L)
    y <- 0L
    ok <- isLessThan(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreLessThan(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isLessThanOrEqualTo : TRUE", {
    x <- seq_len(2L)
    y <- 3L
    ok <- isLessThanOrEqualTo(x = x, y = y)
    expect_true(all(ok))
    ok <- allAreLessThanOrEqualTo(x = x, y = y)
    expect_true(ok)
})

test_that("isLessThanOrEqualTo : FALSE", {
    x <- seq_len(2L)
    y <- 0L
    ok <- isLessThanOrEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreLessThanOrEqualTo(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})
