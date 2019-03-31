context("isEqual")

# nolint start

test_that("TRUE", {
    expect_true(isEqualTo(x = 1L, y = 1))
    expect_true(isNotEqualTo(x = 2, y = 1))
    expect_true(isGreaterThan(x = 1, y = 0))
    expect_true(all(isGreaterThanOrEqualTo(x = seq_len(2), y = 1)))
    expect_true(isLessThan(x = -1, y = 0))
    expect_true(all(isLessThanOrEqualTo(x = seq_len(2), y = 3)))
})

test_that("FALSE", {
    ok <- isEqualTo(x = seq_len(2), y = 1)
    expect_identical(nocause(ok), c(`1` = TRUE, `2` = FALSE))
    expect_identical(cause(ok), noquote(c("", "not equal to 1; abs diff = 1")))
})

# nolint end
