context("check : scalar : areSameLength")

test_that("TRUE", {
    x <- list(a = 1L, b = 2L)
    y <- list(c = 3L, d = 4L)
    ok <- areSameLength(x = x, y = y)
    expect_true(ok)
})

test_that("FALSE", {
    x <- list(a = 1L)
    y <- list(b = 2L, c = 3L)
    ok <- areSameLength(x = x, y = y)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        "{.var x} doesn't have the same length as {.var y}."
    )
})
