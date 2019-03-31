context("validate")

test_that("Success", {
    expect_true(
        validate(is.integer(1L))
    )
})

test_that("Failure", {
    expect_identical(
        validate(is.character(1L)),
        "is.character(1L) is not TRUE."
    )
})

test_that("Custom message", {
    expect_identical(
        validate(is.character(1L), msg = "custom message"),
        "custom message"
    )
})
