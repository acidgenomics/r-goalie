context("Assert")

test_that("assert", {
    expect_silent(assert(is.character("xxx")))
    expect_error(assert(is.character(1L)))
    expect_silent(assert(is.integer(1L), is.numeric(1L)))
})
