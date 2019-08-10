context("isInt")

## `isInt()` is a scalar short alias of `isIntegerish()`.

test_that("TRUE", {
    expect_true(isInt(1L))
    expect_true(isInt(1.0))
    expect_true(isInt(1))  # nolint
})

test_that("FALSE : not integer", {
    ok <- isInt(0.1)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote(c("1.000000000000000e-01" = "not integer"))
    )
})

test_that("FALSE : not scalar", {
    ok <- isInt(seq_len(2L))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("'x' does not have a length of 1.")
    )
})
