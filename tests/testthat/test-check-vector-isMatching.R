context("isMatching")

test_that("TRUE", {
    expect_true(isMatchingRegex(x = "foobar", pattern = "^f"))
    expect_true(isNotMatchingRegex(x = "foobar", pattern = "^b"))
})

test_that("FALSE", {
    expect_true(isMatchingFixed(x = "foobar", pattern = "bar"))
    expect_true(isNotMatchingFixed(x = "foo", pattern = "bar"))
})
