context("check : scalar : allAreAtomic")

test_that("TRUE", {
    expect_true(allAreAtomic(data.frame(a = "foo", b = "bar")))
    expect_true(allAreAtomic(list(a = "foo", b = "bar")))
})

test_that("FALSE : non-atomic", {
    ok <- allAreAtomic(list(a = "x", b = list()))
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        "Not all elements in 'list(a = \"x\", b = list())' are atomic."
    )
})

test_that("FALSE : length 0", {
    ok <- allAreAtomic(data.frame())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        "'data.frame()' has length 0."
    )
})
