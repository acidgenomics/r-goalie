context("setCause")

test_that("FALSE", {
    ok <- setCause(x = FALSE, false = "xxx")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("xxx"))
})

test_that("NA logical (missing)", {
    ok <- setCause(
        x = c(a = TRUE, b = FALSE, c = NA),
        false = "custom false",
        missing = "custom missing"
    )
    expect_s3_class(ok, "goalie")
    expect_identical(
        nocause(ok),
        c(a = TRUE, b = FALSE, c = NA)
    )
    expect_identical(
        cause(ok),
        noquote(c("", "custom false", "custom missing"))
    )
})
