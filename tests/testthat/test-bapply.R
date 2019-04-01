context("bapply")

test_that("bapply", {
    expect_identical(
        bapply(
            X = list(a = "a", b = 1L),
            FUN = is.character
        ),
        c(a = TRUE, b = FALSE)
    )
})
