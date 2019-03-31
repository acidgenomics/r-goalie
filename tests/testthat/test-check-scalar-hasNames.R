context("hasNames")

test_that("TRUE", {
    expect_true(hasNames(mtcars))
})

test_that("FALSE : matrix", {
    ok <- hasNames(matrix())
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("The names of matrix() are NULL.")
    )
})

test_that("FALSE : data frame", {
    ok <- hasNames(data.frame())
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("The names of data.frame() are all empty.")
    )
})
