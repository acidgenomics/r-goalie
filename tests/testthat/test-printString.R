context("printString")

test_that("printString", {
    ## Check for vector concatenation to string.
    expect_identical(
        printString(c("hello", "world")),
        "[1] \"hello\" \"world\""
    )

    ## Check for proper data frame collapse to string.
    expect_identical(
        printString(mtcars[, seq_len(2L)], max = 2L),
        "                     mpg cyl\nMazda RX4           21.0   6"
    )
})
