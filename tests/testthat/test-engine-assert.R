test_that("Success", {
    expect_true(assert(is.character("xxx")))
    expect_true(assert(is.integer(1L), is.numeric(1L)))
})

test_that("Failure", {
    expect_error(
        object = assert(is.character(1L)),
        regexp = "is.character"
    )
})

test_that("Custom message", {
    expect_error(
        object = assert(is.logical("xxx"), msg = "custom error"),
        regexp = "custom error"
    )
})

test_that("Named arguments", {
    expect_error(
        object = assert(
            "AAA" = TRUE,
            "BBB" = is.logical(1L),
            "CCC" = is.character("AAA")
        ),
        regexp = "BBB"
    )
})

test_that("Not boolean", {
    expect_error(
        object = assert(c(TRUE, TRUE)),
        regexp = "boolean flag"
    )
    expect_error(
        object = assert("xxx"),
        regexp = "boolean flag"
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
