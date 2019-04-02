context("validate")

test_that("Success", {
    expect_true(validate(is.integer(1L)))
})

test_that("Expected failure", {
    expect_identical(
        validate(is.character(1L)),
        "is.character(1L) is not TRUE."
    )
})

test_that("Character passthrough", {
    expect_identical(validate("xxx"), "xxx")
})

test_that("Invalid input", {
    expect_error(
        validate(c(TRUE, FALSE)),
        regexp = "logical\\(1\\)"
    )
    expect_error(
        object = validate(c("aaa", "bbb")),
        regexp = "character\\(1\\)"
    )
})

test_that("Custom message", {
    expect_identical(
        validate(is.character(1L), msg = "custom message"),
        "custom message"
    )
})
