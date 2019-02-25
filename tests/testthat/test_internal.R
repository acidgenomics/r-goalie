context("internal")

test_that("capitalize", {
    expect_identical(
        .capitalize(character()),
        character()
    )
})
