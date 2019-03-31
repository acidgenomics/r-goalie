context("isAll")

x <- 1L

test_that("TRUE", {
    expect_true(isAll(x, classes = c("integer", "numeric")))
})

test_that("FALSE", {
    ok <- isAll(x, classes = c("integer", "NULL"))
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("x is not all: integer, NULL")
    )
})
