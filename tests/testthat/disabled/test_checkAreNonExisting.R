context("Are non-existing?")

test_that("assertAreNonExisting", {
    expect_silent(assertAreNonExisting(
        x = c("a", "b", "c"),
        inherits = FALSE
    ))
    # Error on existing values in environment.
    a <- 1L
    b <- 2L
    expect_error(assertAreNonExisting(
        x = c("a", "b", "c"),
        inherits = FALSE
    ))
})
