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

test_that("Require logical input", {
    expect_error(
        object = print.goalie(NULL),
        regexp = "x is not logical."
    )
})

test_that("cause error check", {
    expect_error(.printGoalieVector("XXX"))
})

test_that("ignoreNA argument", {
    x <- NA
    cause(x) <- "XXX"
    expect_identical(
        capture.output(.printGoalieVector(x, ignoreNA = FALSE))[[1L]],
        "There was 1 failure:"
    )
    expect_identical(
        capture.output(.printGoalieVector(x, ignoreNA = TRUE))[[1L]],
        "There were 0 failures:"
    )
})
