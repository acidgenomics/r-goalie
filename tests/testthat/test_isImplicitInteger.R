context("Is implicit integer (integerish)?")

test_that("isImplicitInteger", {
    expect_false(isImplicitInteger(list(1, 1L, 1.1, "XXX")))  # nolint
})

test_that("assertIsAnImplicitInteger", {
    expect_silent(assertIsAnImplicitInteger(1))  # nolint
    expect_silent(assertIsAnImplicitInteger(1L))
    expect_silent(assertIsAnImplicitInteger(1.0))
    expect_error(assertIsAnImplicitInteger(c(1L, 2L)))
    expect_error(assertIsAnImplicitInteger(1.1))
    # Check tolerance threshold.
    expect_error(assertIsImplicitInteger(1.000000000000001))
})

test_that("assertIsAnImplicitIntegerOrNULL", {
    expect_silent(assertIsAnImplicitIntegerOrNULL(NULL))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1))  # nolint
    expect_silent(assertIsAnImplicitIntegerOrNULL(1L))
    expect_silent(assertIsAnImplicitIntegerOrNULL(1.0))
    expect_error(assertIsAnImplicitIntegerOrNULL(1.1))
})
