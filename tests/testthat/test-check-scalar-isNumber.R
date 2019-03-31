context("check : scalar : isNumber")

test_that("TRUE", {
    expect_true(isNumber(0))  # nolint
    expect_true(isNumber(1L))
    expect_true(isNumber(1.1))
})

test_that("FALSE", {
    expect_false(isNumber(seq_len(2L)))
})
