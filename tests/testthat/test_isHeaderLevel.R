context("Is Markdown header level?")

test_that("assertIsHeaderLevel", {
    expect_silent(assertIsHeaderLevel(1L))
    expect_error(
        object = assertIsHeaderLevel(8L),
        regexp = "not a valid Markdown header level"
    )
})
