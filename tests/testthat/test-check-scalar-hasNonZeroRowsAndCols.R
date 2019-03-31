context("check : scalar : hasNonZeroRowsAndCols")

test_that("TRUE", {
    x <- matrix(data = seq_len(4L), nrow = 2L)
    expect_true(hasNonZeroRowsAndCols(x))

    x <- matrix(data = rep(1L, times = 2L), byrow = TRUE)
    expect_true(hasNonZeroRowsAndCols(x))

    x <- matrix(data = rep(1L, times = 2L), byrow = FALSE)
    expect_true(hasNonZeroRowsAndCols(x))
})

test_that("FALSE : no rows", {
    x <- matrix(nrow = 0L, ncol = 1L)
    ok <- hasNonZeroRowsAndCols(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("The number of rows in x is zero.")
    )
})

test_that("FALSE : no columns", {
    x <- matrix(nrow = 1L, ncol = 0L)
    ok <- hasNonZeroRowsAndCols(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("The number of columns in x is zero.")
    )
})

test_that("FALSE : no rows or columns", {
    x <- matrix(nrow = 0L, ncol = 0L)
    ok <- hasNonZeroRowsAndCols(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("The number of rows in x is zero.")
    )
})
