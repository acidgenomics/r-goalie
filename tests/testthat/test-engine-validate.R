context("engine : validate")

test_that("Success", {
    expect_true(validate(is.integer(1L)))
})

test_that("Expected failure", {
    expect_match(
        object = validate(is.character(1L)),
        regexp = "is.character\\(1L\\) is not TRUE."
    )
})

test_that("Character passthrough", {
    expect_identical(validate("xxx"), "xxx")
    expect_match(
        object = validate("foo", "bar"),
        regexp = "foo\nbar"
    )
    expect_error(
        object = validate(c("foo", "bar")),
        regexp = "length\\(r\\) == 1L is not TRUE"
    )
})

test_that("Invalid input", {
    expect_error(validate())
    expect_error(validate(c(TRUE, FALSE)))
    expect_error(validate(c("aaa", "bbb")))
    expect_error(validate(NA_integer_))
})

test_that("Custom message", {
    expect_identical(
        object = validate(is.character(1L), msg = "custom message"),
        expected = "custom message"
    )
})

test_that("goalie cause support", {
    expect_match(
        object = validate(isFlag("XXX")),
        regexp = "boolean flag"
    )
    expect_match(
        object = validate(isADir("XXX")),
        regexp = "Cause: XXX: not dir"
    )
})
