context("engine : bapply")

test_that("bapply", {
    expect_identical(
        object = bapply(
            X = list(a = "a", b = 1L),
            FUN = is.character
        ),
        expected = c("a" = TRUE, "b" = FALSE)
    )
})
