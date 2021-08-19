context("engine : assert")

test_that("Success", {
    expect_true(assert(is.character("xxx")))
    expect_true(assert(is.integer(1L), is.numeric(1L)))
})

test_that("Failure", {
    expect_error(
        object = assert(is.character(1L)),
        regexp = paste(
            "Assert failure.",
            "\\[1\\] is.character\\(1L\\) is not TRUE.",
            sep = "\n"
        )
    )
})

test_that("Custom message", {
    expect_error(
        object = assert(is.logical("xxx"), msg = "custom error"),
        regexp = "custom error"
    )
})

test_that("Not boolean", {
    expect_error(
        object = assert(c(TRUE, TRUE)),
        regexp = "Check did not return a boolean flag \\(TRUE/FALSE\\)."
    )
    expect_error(
        object = assert("xxx"),
        regexp = "Check did not return a boolean flag \\(TRUE/FALSE\\)."
    )
})

test_that("Invalid input, checking stop passthrough", {
    expect_error(
        object = assert(stop("XXX")),
        regexp = "XXX"
    )
})

test_that("Error on empty assert call", {
    expect_error(assert())
})

test_that("goalie cause support", {
    expect_error(
        object = assert(isFlag("XXX")),
        regexp = "Cause:"
    )
    expect_error(
        object = assert(isADir("XXX")),
        regexp = "Cause: XXX: not dir"
    )
})
