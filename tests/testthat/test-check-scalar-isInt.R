## `isInt()` is a scalar short alias of `isIntegerish()`.

test_that("TRUE", {
    expect_true(isInt(1L))
    expect_true(isInt(1.0))
    expect_true(isInt(1)) # nolint
})

test_that("FALSE : not integer", {
    ok <- isInt(0.1)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "not integer"
    )
})

test_that("FALSE : not scalar", {
    ok <- isInt(seq_len(2L))
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var integer} doesn't have a length of 1."
    )
})
