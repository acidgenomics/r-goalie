context("check : scalar : hasDims")

test_that("hasDims", {
    expect_true(hasDims(mtcars))
    ## Note that dims don't have to be non-zero, just not NULL.
    expect_true(hasDims(data.frame()))
    ok <- hasDims(list())
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "The dimensions of {.var list()} are {.val NULL}."
    )
})

test_that("hasRows, hasCols", {
    for (fun in list(hasRows, hasCols)) {
        x <- mtcars
        expect_true(fun(x))
        x <- data.frame()
        expect_false(fun(x))
        ## Support NULL.
        x <- list()
        expect_false(fun(x))
    }
})
