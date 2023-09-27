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

test_that("nullOk", {
    expect_false(isString(NULL, nullOk = FALSE))
    expect_true(isString(NULL, nullOk = TRUE))
})
