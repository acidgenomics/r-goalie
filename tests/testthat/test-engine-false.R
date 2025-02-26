test_that("cause", {
    ok <- false("xxx")
    expect_s4_class(ok, "goalie")
    expect_false(nocause(ok))
    expect_identical(cause(ok), "xxx")
})

test_that("empty cause", {
    expect_error(false())
})
