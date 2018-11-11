context("Is implicit integer (integerish)?")

test_that("isImplicitInteger", {
    expect_true(isImplicitInteger(c(1, 2)))  # nolint
    expect_false(isImplicitInteger(c(1.1, 2.1)))
    expect_false(isImplicitInteger("XXX"))
})

test_that("assertIsImplicitInteger", {
    expect_silent(assertIsImplicitInteger(c(1, 2)))  # nolint
    expect_silent(assertIsImplicitInteger(c(1L, 2L)))
    expect_silent(assertIsImplicitInteger(c(1.0, 2.0)))
    expect_error(assertIsImplicitInteger(c(1.1, 2.1)))
})

test_that("assertIsImplicitIntegerOrNULL", {
    expect_silent(assertIsImplicitIntegerOrNULL(NULL))
    expect_silent(assertIsImplicitIntegerOrNULL(c(1, 2)))  # nolint
    expect_silent(assertIsImplicitIntegerOrNULL(c(1L, 2L)))
    expect_silent(assertIsImplicitIntegerOrNULL(c(1.0, 2.0)))
    expect_error(assertIsImplicitIntegerOrNULL(c(1.1, 2.1)))
})

test_that("isAnImplicitInteger", {
    expect_true(isAnImplicitInteger(1))  # nolint
    expect_true(isAnImplicitInteger(1L))
    expect_true(isAnImplicitInteger(1.0))
    expect_false(isAnImplicitInteger(1.1))
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
