test_that("TRUE", {
    expect_identical(
        object = isIntegerish(seq_len(2L)),
        expected = rep(TRUE, 2L)
    )
    expect_identical(
        object = isIntegerish(c(1, 2)), # nolint
        expected = rep(TRUE, 2L)
    )
    expect_identical(
        object = isIntegerish(S4Vectors::Rle(c(1, 2))), # nolint
        expected = rep(TRUE, 2L)
    )
})

test_that("FALSE", {
    ok <- isIntegerish(0.1)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "not integer"
    )
})

test_that("FALSE : NA input", {
    ok <- isIntegerish(c(1, 2, NA)) # nolint
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = nocause(ok),
        expected = c(TRUE, TRUE, FALSE)
    )
    expect_identical(
        object = cause(ok),
        expected = c(
            rep(NA_character_, 2L),
            "not integer"
        )
    )
})

test_that("nullOk", {
    expect_false(isInt(NULL, nullOk = FALSE))
    expect_true(isInt(NULL, nullOk = TRUE))
})

test_that("allAreIntegerish", {
    x <- c(1, 2) # nolint
    expect_true(allAreIntegerish(x))
    x <- c(1, 1.1) # nolint
    expect_false(allAreIntegerish(x))
})
