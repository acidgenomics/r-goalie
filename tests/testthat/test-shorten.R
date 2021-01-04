context("shorten")

test_that("shorten", {
    expect_identical(
        object = shorten(c("AAAAA", "BBBB"), width = 4L),
        expected = c("A...", "BBBB")
    )
})
