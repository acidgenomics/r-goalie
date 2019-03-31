context("check : scalar : hasElements")

test_that("TRUE", {
    expect_true(hasElements("hello", n = 1L))
    expect_true(hasElements(list(a = 1L, b = 2L), n = 2L))
})

test_that("FALSE", {
    ok <- hasElements(list(), n = 1L)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote("list() has 0 elements, not 1.")
    )
})
