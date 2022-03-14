context("check : scalar : isHeaderLevel")

test_that("TRUE", {
    expect_true(isHeaderLevel(1)) # nolint
    expect_true(isHeaderLevel(7L))
})

test_that("FALSE", {
    expect_false(isHeaderLevel(seq_len(7L)))
    expect_false(isHeaderLevel(0L))
})
