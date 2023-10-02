x <- 1L

test_that("TRUE", {
    expect_true(isAll(x, classes = c("integer", "numeric")))
})

test_that("FALSE", {
    ok <- isAll(x, classes = c("integer", "NULL"))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var 1} is not all of: integer, NULL."
    )
})
