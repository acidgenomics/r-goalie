context("isAlpha")

test_that("TRUE", {
    expect_true(isAlpha(0.05))
    expect_true(isAlpha(1e-10))
})

test_that("FALSE : out of bounds", {
    ok <- isAlpha(0)  # nolint
    expect_identical(
        cause(ok),
        noquote(c("0.000000000000000e+00" = "too low"))
    )
    ok <- isAlpha(1)  # nolint
    expect_identical(
        cause(ok),
        noquote(c("1.000000000000000e+00" = "too high"))
    )
})

test_that("FALSE : not scalar double", {
    ok <- isAlpha(c(0.1, 0.1))
    expect_identical(
        cause(ok),
        noquote("'c(0.1, 0.1)' is not scalar double.")
    )
})

test_that("FALSE : zero integer", {
    ok <- isAlpha(0L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("'0L' is not scalar double.")
    )
})
