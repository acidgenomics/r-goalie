context("print")

## Here we want to capture the print method on goalie S3 class.
## These differ depending on whether the assert check is scalar or vectorized.

test_that("scalar", {
    expect_output(
        object = print(isFlag(1L)),
        regexp = "'1L' is not a boolean flag"
    )
})

test_that("vector", {
    expect_output(
        object = print(isIntegerish(c(1L, NA))),
        regexp = "1   2    NA    NA"
    )
})

test_that("n arg", {
    out <- capture.output(print(isFile(c("AAA", "BBB", "CCC")), n = 2L))
    expect_identical(
        object = out[[1L]],
        expected = "[1] FALSE FALSE"
    )
})

test_that("ignoreNA arg", {
    x <- c(a = TRUE, b = FALSE, c = NA)
    cause(x) <- c(a = "TRUE", b = "FALSE", c = "NA")

    out <- capture.output(print(x, ignoreNA = FALSE))
    expect_length(out, n = 5L)
    expect_match(out[[4L]], "FALSE")
    expect_match(out[[5L]], "NA")

    out <- capture.output(print(x, ignoreNA = TRUE))
    expect_length(out, n = 4L)
    expect_match(out[[4L]], "FALSE")
})
