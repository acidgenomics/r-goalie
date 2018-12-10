context("Has rownames?")

library(S4Vectors)
as_tibble <- tibble::as_tibble  # nolint

test_that("assertHasRownames", {
    df <- data.frame(
        x = seq_len(2L),
        y = seq_len(2L),
        row.names = letters[seq_len(2L)]
    )
    expect_silent(assertHasRownames(df))
    expect_silent(assertHasRownames(as(df, "DataFrame")))
    expect_silent(assertHasRownames(as_tibble(df, rownames = "rowname")))

    expect_error(assertHasRownames(DataFrame()))
    expect_error(assertHasRownames(data.frame()))
    expect_error(assertHasRownames(tibble()))
})
