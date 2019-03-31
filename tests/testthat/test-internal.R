context("Internal functions")

test_that("capitalize", {
    expect_identical(
        .capitalize(character()),
        character()
    )
})
