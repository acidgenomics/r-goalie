context("isAlpha")

test_that("TRUE", {
    expect_true(isAlpha(0.05))
    expect_true(isAlpha(1e-10))
})

test_that("FALSE : out of bounds", {
    expect_identical(cause(isAlpha(0)), noquote("too low"))  # nolint
    expect_identical(cause(isAlpha(1)), noquote("too high"))  # nolint
})

test_that("FALSE : not scalar double", {
    expect_identical(
        cause(isAlpha(c(0.1, 0.1))),
        noquote("c(0.1, 0.1) is not scalar double.")
    )
})

test_that("FALSE : zero integer", {
    ok <- isAlpha(0L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("0L is not scalar double.")
    )
})
