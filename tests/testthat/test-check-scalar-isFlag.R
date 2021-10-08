context("check : scalar : isFlag")

test_that("TRUE", {
    expect_true(isFlag(TRUE))
    expect_true(isFlag(FALSE))
})

test_that("FALSE : logical but not boolean", {
    ok <- isFlag(c(TRUE, TRUE))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = paste(
            "{.var c(TRUE, TRUE)} is not a boolean flag",
            "({.val TRUE}/{.val FALSE})."
        )
    )
})

test_that("FALSE : integer", {
    ok <- isFlag(1L)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = paste(
            "{.var 1L} is not a boolean flag",
            "({.val TRUE}/{.val FALSE})."
        )
    )
})

test_that("FALSE : NA is logical but not boolean", {
    ok <- isFlag(NA)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var NA} is {.val NA}."
    )
})
