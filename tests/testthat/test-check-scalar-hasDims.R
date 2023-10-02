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
        expected = "The dimensions of {.var list} are {.val NULL}."
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
    ok <- hasDims(x, n = c(2L, 3L))
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = paste(
            "Dimension mismatch for {.var matrix}:",
            "expected {.val 2:3}; actual {.val 3:2}."
        )
    )
    expect_true(hasRows(x, n = 3L))
    expect_true(hasRows(x, n = 3)) # nolint
    ok <- hasRows(x, n = 2L)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = paste(
            "Row number mismatch for {.var matrix}:",
            "expected {.val 2}; actual {.val 3}."
        )
    )
    expect_true(hasCols(x, n = 2L))
    expect_true(hasCols(x, n = 2)) # nolint
    ok <- hasCols(x, n = 3L)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = paste(
            "Column number mismatch for {.var matrix}:",
            "expected {.val 3}; actual {.val 2}."
        )
    )
})
