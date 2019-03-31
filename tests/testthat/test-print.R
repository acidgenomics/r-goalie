context("print : goalie")

# Here we want to capture the print method on goalie S3 class.
# These differ depending on whether the assert check is scalar or vectorized.

test_that("scalar", {
    expect_identical(
        capture.output(print(isFlag(1L))),
        c(
            "[1] FALSE",
            "Cause of failure:",
            "1L is not a boolean flag (TRUE/FALSE)."
        )
    )
})

test_that("vector", {
    expect_identical(
        capture.output(print(isIntegerish(c(1L, NA)))),
        c(
            "There was 1 failure:",
            "  pos value cause",
            "1   2          NA"
        )
    )
})
