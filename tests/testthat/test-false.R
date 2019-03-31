context("false")

test_that("cause", {
    ok <- false("xxx")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("xxx"))
})

test_that("empty cause", {
    ok <- false()
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote(""))
})

