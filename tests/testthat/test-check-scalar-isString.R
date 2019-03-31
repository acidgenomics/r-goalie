context("isString")

test_that("TRUE", {
    expect_true(isString("hello world"))
})

test_that("FALSE", {
    expect_false(isString(c("hello", "world")))
    expect_false(isString(NULL))
    expect_false(isString(1L))
    expect_false(isString(""))
    expect_false(isString(NA_character_))
})
