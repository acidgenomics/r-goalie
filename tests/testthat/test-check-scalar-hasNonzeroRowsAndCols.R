context("check : scalar : hasNonzeroRowsAndCols")

test_that("TRUE : matrix", {
    x <- matrix(data = seq_len(4L), nrow = 2L)
    expect_true(hasNonzeroRowsAndCols(x))
    x <- matrix(data = rep(1L, times = 2L), byrow = TRUE)
    expect_true(hasNonzeroRowsAndCols(x))
    x <- matrix(data = rep(1L, times = 2L), byrow = FALSE)
    expect_true(hasNonzeroRowsAndCols(x))
})

test_that("TRUE : sparseMatrix", {
    skip_if_not_installed("Matrix")
    x <- Matrix::sparseMatrix(
        i = seq_len(4L),
        j = seq_len(4L),
        x = 1L
    )
    expect_true(hasNonzeroRowsAndCols(x))
})

test_that("FALSE : rows containing all zeros", {
    skip_if_not_installed("Matrix")
    ## This example is from the sparseMatrix documentation.
    x <- Matrix::sparseMatrix(
        i = c(1L, 3L:8L),
        j = c(2L, 9L, 6L:10L),
        x = 7L * (seq_len(7L))
    )
    ok <- hasNonzeroRowsAndCols(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var x} has 1 zero row at position 2."
    )
})

test_that("FALSE : columns containing all zeros", {
    x <- matrix(
        data = c(rep(1L, times = 8L), rep(0L, times = 8L)),
        nrow = 4L, ncol = 4L,
        byrow = FALSE
    )
    ok <- hasNonzeroRowsAndCols(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var x} has 2 zero columns at positions 3, 4."
    )
})

test_that("FALSE : no rows", {
    x <- matrix(nrow = 0L, ncol = 1L)
    ok <- hasNonzeroRowsAndCols(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "The number of rows in {.var x} is zero."
    )
})

test_that("FALSE : no columns", {
    x <- matrix(nrow = 1L, ncol = 0L)
    ok <- hasNonzeroRowsAndCols(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "The number of columns in {.var x} is zero."
    )
})

test_that("FALSE : no rows or columns", {
    x <- matrix(nrow = 0L, ncol = 0L)
    ok <- hasNonzeroRowsAndCols(x)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "The number of rows in {.var x} is zero."
    )
})
