context("engine : validate")

test_that("Success", {
    expect_true(validate(is.integer(1L)))
})

test_that("Expected failure", {
    expect_identical(
        object = validate(is.character(1L)),
        expected = "is.character(1L) is not TRUE."
    )
})

test_that("Character passthrough", {
    expect_identical(validate("xxx"), "xxx")
})

test_that("Invalid input", {
    expect_error(
        object = validate(c(TRUE, FALSE)),
        regexp = "logical\\(1\\)"
    )
    expect_error(
        object = validate(c("aaa", "bbb")),
        regexp = "character\\(1\\)"
    )
})

test_that("Custom message", {
    expect_identical(
        object = validate(is.character(1L), msg = "custom message"),
        expected = "custom message"
    )
})

test_that("Error on empty validate call", {
    expect_error(validate())
})

test_that("goalie cause support", {
    expect_match(
        object = validate(isFlag("XXX")),
        regexp = "Cause:"
    )
})
