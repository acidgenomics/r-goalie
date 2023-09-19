test_that("hasDims", {
    x <- matrix(data = seq(from = 1L, to = 6L), nrow = 3L, ncol = 2L)
    expect_true(hasDims(x))
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
        x <- matrix(data = seq(from = 1L, to = 6L), nrow = 3L, ncol = 2L)
        expect_true(fun(x))
        x <- data.frame()
        expect_false(fun(x))
        ## Support NULL.
        x <- list()
        expect_false(fun(x))
    }
})

test_that("n length support", {
    x <- matrix(data = seq(from = 1L, to = 6L), nrow = 3L, ncol = 2L)
    expect_true(hasDims(x, n = c(3L, 2L)))
    expect_true(hasDims(x, n = c(3, 2))) # nolint
    expect_true(hasRows(x, n = 3L))
    expect_true(hasCols(x, n = 2L))
    expect_error(hasDims(x, n = 1L))

    expect_error(hasRows(x, n = 1)) # nolint
    expect_error(hasCols(x, n = 1)) # nolint
})
