context("setCause")

test_that("FALSE", {
    ok <- setCause(x = FALSE, false = "xxx")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("xxx"))
})

test_that("NA logical (missing)", {
    ok <- setCause(x = NA, missing = "xxx")
    expect_s3_class(ok, "goalie")
    expect_true(is.na(ok))
    expect_identical(cause(ok), noquote("xxx"))
})
