context("isAny")

x <- 1L

test_that("TRUE", {
    expect_true(isAny(x, classes = c("integer", "NULL")))
    expect_true(isAny(x, classes = c("numeric", "NULL")))
    expect_true(isAny(x, classes = c("atomic", "NULL")))
})

test_that("FALSE", {
    ok <- isAny(x, classes = c("character", "data.frame"))
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("x is not any of: character, data.frame.")
    )
})
