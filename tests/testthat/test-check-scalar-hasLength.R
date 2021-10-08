context("check : scalar : hasLength")

test_that("TRUE", {
    expect_true(hasLength(seq(2L), n = 2L))
    expect_true(hasLength(1L))
    expect_true(hasLength(FALSE))
    expect_true(hasLength(mtcars))
    expect_true(hasLength(""))
})

test_that("FALSE : not expected length", {
    ok <- hasLength(x = "xxx", n = 2L)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    cause(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var \"xxx\"} doesn't have a length of 2."
    )
})

test_that("FALSE : NULL", {
    ok <- hasLength(NULL)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var NULL} has length 0."
    )
})

test_that("FALSE : empty character", {
    ok <- hasLength(character())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var character()} has length 0."
    )
})

test_that("FALSE : empty data frame", {
    ok <- hasLength(data.frame())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var data.frame()} has length 0."
    )
})
