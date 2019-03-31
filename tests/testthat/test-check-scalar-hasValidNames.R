context("hasValidNames")

test_that("TRUE", {
    x <- list(a = 1L, b = 2L)
    expect_true(hasValidNames(x))
})

test_that("FALSE", {
    x <- list(
        `1`       = 1L,  # can't start with number
        `foo bar` = 2L,  # no spaces
        `foo-bar` = 3L   # no hyphens
    )
    ok <- hasValidNames(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("x does not have valid names.")
    )
})



context("hasValidDimnames")

test_that("TRUE", {
    x <- iris
    expect_true(hasValidDimnames(x))
})

test_that("FALSE", {
    # Note the spaces in the row names here.
    x <- mtcars
    ok <- hasValidDimnames(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("x has invalid row names.")
    )
})
