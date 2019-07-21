context("hasDims")

test_that("hasDims", {
    expect_true(hasDims(mtcars))

    ## Note that dims don't have to be non-zero, just not NULL.
    expect_true(hasDims(data.frame()))

    ok <- hasDims(list())
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("The dimensions of list() are NULL.")
    )
})

with_parameters_test_that(
    "hasRows, hasCols", {
        x <- mtcars
        expect_true(fun(x))

        x <- data.frame()
        expect_false(fun(x))

        ## Support NULL.
        x <- list()
        expect_false(fun(x))
    },
    fun = list(hasRows, hasCols)
)
