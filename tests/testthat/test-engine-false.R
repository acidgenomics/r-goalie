context("engine : false")

test_that("cause", {
    ok <- false("xxx")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), "xxx")
})

test_that("empty cause", {
    ok <- false()
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), "")
})
