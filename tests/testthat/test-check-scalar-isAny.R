context("check : scalar : isAny")

x <- 1L

test_that("TRUE", {
    expect_true(isAny(x, classes = c("integer", "NULL")))
    expect_true(isAny(x, classes = c("numeric", "NULL")))
    expect_true(isAny(x, classes = c("atomic", "NULL")))
})

test_that("FALSE", {
    ok <- isAny(x, classes = c("character", "data.frame"))
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var x} is not any of: character, data.frame."
    )
})

test_that("Invalid classes argument", {
    ok <- isAny("XXX", classes = NULL)
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "{.var classes} is not character."
    )
})
