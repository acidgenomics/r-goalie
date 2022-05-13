test_that("TRUE", {
    expect_true(allAreAtomic(data.frame(a = "foo", b = "bar")))
    expect_true(allAreAtomic(list(a = "foo", b = "bar")))
})

test_that("FALSE : non-atomic", {
    ok <- allAreAtomic(list(a = "x", b = list()))
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = paste(
            "Not all elements in {.var list(a = \"x\", b = list())}",
            "are atomic."
        )
    )
})

test_that("FALSE : length 0", {
    ok <- allAreAtomic(data.frame())
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var data.frame()} has length 0."
    )
})
