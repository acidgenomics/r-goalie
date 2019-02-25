context("print")



# Here we want to capture the print method on goalie S3 class.
# These differ depending on whether the assert check is scalar or vectorized.

test_that("print : scalar", {
    expect_identical(
        capture.output(print(isFlag(1L))),
        c(
            "[1] FALSE",
            "Cause of failure:",
            "1L is not a boolean flag (TRUE/FALSE)."
        )
    )
})

test_that("print : vector", {
    expect_identical(
        capture.output(print(isIntegerish(c(1L, NA)))),
        c(
            "There was 1 failure:",
            "  pos value cause",
            "1   2          NA"
        )
    )
})



test_that("printString", {
    # Check for vector concatenation to string.
    expect_identical(
        printString(c("hello", "world")),
        "[1] \"hello\" \"world\""
    )

    # Check for proper data frame collapse to string.
    expect_identical(
        printString(datasets::mtcars[, seq_len(2L)], max = 2L),
        "                     mpg cyl\nMazda RX4           21.0   6"
    )
})
