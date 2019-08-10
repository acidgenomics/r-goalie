context("toNames")

test_that("Non character", {
    expect_identical(toNames(1L), "1")
    ## Return numerics in scientific notation.
    expect_identical(
        object = toNames(c(1, 1.1E-4)),  # nolint
        expected = c(
            "1.000000000000000e+00",
            "1.100000000000000e-04"
        )
    )
    expect_identical(toNames(complex(1L)), "0+0i")
    expect_identical(toNames(NA), "NA")
    expect_identical(toNames(TRUE), "TRUE")
})

## Doesn't use `make.names()` to sanitize.
test_that("Character input of invalid names", {
    x <- c("sample-1", "hello world")
    expect_identical(toNames(x), x)
})
