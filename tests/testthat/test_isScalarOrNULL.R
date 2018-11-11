context("Is scalar (or NULL)?")

test_that("assertIsAnIntegerOrNULL", {
    expect_silent(assertIsAnIntegerOrNULL(1L))
    expect_silent(assertIsAnIntegerOrNULL(NULL))
    expect_error(
        object = assertIsAnIntegerOrNULL(c(1L, 2L)),
        regexp = "is_an_integer : x has length 2, not 1."
    )
})

test_that("assertIsANumberOrNULL", {
    expect_silent(assertIsANumberOrNULL(1.1))
    expect_silent(assertIsANumberOrNULL(NULL))
    expect_error(
        object = assertIsANumberOrNULL(c(1.1, 1.2)),
        regexp = "is_a_number : x has length 2, not 1."
    )
})

test_that("assertIsStringOrNULL", {
    expect_silent(assertIsStringOrNULL("hello world"))
    expect_silent(assertIsStringOrNULL(NULL))
    expect_error(
        object = assertIsStringOrNULL(c("hello", "world")),
        regexp = "is_a_string : x has length 2, not 1."
    )
})
