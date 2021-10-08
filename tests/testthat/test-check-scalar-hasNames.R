context("check : scalar : hasNames")

test_that("TRUE", {
    expect_true(hasNames(mtcars))
})

test_that("FALSE : matrix", {
    ok <- hasNames(matrix())
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        cause(ok),
        "The names of {.var matrix()} are {.val NULL}."
    )
})

test_that("FALSE : data frame", {
    ok <- hasNames(data.frame())
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        cause(ok),
        "The names of {.var data.frame()} are all empty."
    )
})
