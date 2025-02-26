test_that("TRUE", {
    expect_true(isString("hello world"))
})

test_that("FALSE", {
    expect_false(nocause(isString(c("hello", "world"))))
    expect_false(nocause(isString(NULL)))
    expect_false(nocause(isString(1L)))
    expect_false(nocause(isString("")))
    expect_false(nocause(isString(NA_character_)))
})

test_that("nullOk", {
    expect_false(nocause(isString(NULL, nullOk = FALSE)))
    expect_true(isString(NULL, nullOk = TRUE))
})
