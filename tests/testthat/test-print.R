context("print")

# Here we want to capture the print method on goalie S3 class.
# These differ depending on whether the assert check is scalar or vectorized.

test_that("scalar", {
    expect_identical(
        object = capture.output(print(isFlag(1L))),
        expected = c(
            "[1] FALSE",
            "Cause of failure:",
            "1L is not a boolean flag (TRUE/FALSE)."
        )
    )
})

test_that("vector", {
    expect_identical(
        object = capture.output(print(isIntegerish(c(1L, NA)))),
        expected = c(
            "There was 1 failure:",
            "  pos value cause",
            "1   2          NA"
        )
    )
})

test_that("Require logical input", {
    expect_error(
        object = print.goalie(NULL),
        regexp = "x is not logical."
    )
})

test_that("cause error check", {
    expect_error(
        object = .print.goalie.vector("XXX"),
        regexp = "cause error."
    )
})

test_that("ignoreNA argument", {
    x <- NA
    cause(x) <- "XXX"
    expect_identical(
        capture.output(.print.goalie.vector(x, ignoreNA = FALSE))[[1L]],
        "There was 1 failure:"
    )
    expect_identical(
        capture.output(.print.goalie.vector(x, ignoreNA = TRUE))[[1L]],
        "There were 0 failures:"
    )
})
