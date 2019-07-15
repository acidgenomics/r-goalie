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

test_that("nullOK", {
    expect_false(isString(NULL, nullOK = FALSE))
    expect_true(isString(NULL, nullOK = TRUE))
})
