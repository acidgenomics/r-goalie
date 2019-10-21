context("check : scalar : isFileSystemCaseSensitive")

## Don't assume case sensitive or insensitive here, otherwise build check will
## fail depending on the platform.
test_that("Simple check", {
    x <- isFileSystemCaseSensitive()
    expect_type(x, "logical")
})
