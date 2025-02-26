test_that("TRUE", {
    expect_true(isHeaderLevel(1)) # nolint
    expect_true(isHeaderLevel(7L))
})

test_that("FALSE", {
    expect_false(nocause(isHeaderLevel(seq_len(7L))))
    expect_false(nocause(isHeaderLevel(0L)))
})
