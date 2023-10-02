test_that("TRUE", {
    expect_true(isAlpha(0.05))
    expect_true(isAlpha(1e-10))
})

test_that("FALSE : out of bounds", {
    ok <- isAlpha(0) # nolint
    expect_identical(
        object = cause(ok),
        expected = "too low"
    )
    ok <- isAlpha(1) # nolint
    expect_identical(
        object = cause(ok),
        expected = "too high"
    )
})

test_that("FALSE : not scalar double", {
    ok <- isAlpha(c(0.1, 0.1))
    expect_identical(
        object = cause(ok),
        expected = "{.var numeric} is not scalar double."
    )
})

test_that("FALSE : zero integer", {
    ok <- isAlpha(0L)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var 0} is not scalar double."
    )
})
