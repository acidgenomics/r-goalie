context("validate")

test_that("validate", {
    expect_true(
        validate(is.integer(1L))
    )
    expect_identical(
        validate(is.character(1L)),
        "is.character(1L) is not TRUE."
    )

})
