test_that("isMatchingFixed : TRUE", {
    x <- c("foobar", "bar")
    pattern <- "bar"
    ok <- isMatchingFixed(x = x, pattern = pattern)
    expect_true(all(ok))
    ok <- allAreMatchingFixed(x = x, pattern = pattern)
    expect_true(ok)
})

test_that("isMatchingFixed : FALSE", {
    x <- c("aaa", "bbb")
    pattern <- "ccc"
    ok <- isMatchingFixed(x = x, pattern = pattern)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreMatchingFixed(x = x, pattern = pattern)
    expect_false(ok)
})

test_that("isMatchingRegex : TRUE", {
    x <- c("foobar", "foo")
    pattern <- "^f"
    ok <- isMatchingRegex(x = x, pattern = pattern)
    expect_true(all(ok))
    ok <- allAreMatchingRegex(x = x, pattern = pattern)
    expect_true(ok)
})

test_that("isMatchingRegex : FALSE", {
    x <- c("foobar", "foo")
    pattern <- "^F"
    ok <- isMatchingRegex(x = x, pattern = pattern)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreMatchingRegex(x = x, pattern = pattern)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isNotMatchingFixed : TRUE", {
    x <- c("aaa", "bbb")
    pattern <- "ccc"
    ok <- isNotMatchingFixed(x = x, pattern = pattern)
    expect_true(all(ok))
    ok <- allAreNotMatchingFixed(x = x, pattern = pattern)
    expect_true(ok)
})

test_that("isNotMatchingFixed : FALSE", {
    x <- c("foobar", "bar")
    pattern <- "bar"
    ok <- allAreNotMatchingFixed(x = x, pattern = pattern)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreNotMatchingFixed(x = x, pattern = pattern)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("isNotMatchingRegex : TRUE", {
    x <- c("foobar", "foo")
    pattern <- "^b"
    ok <- isNotMatchingRegex(x = x, pattern = pattern)
    expect_true(all(ok))
    ok <- allAreNotMatchingRegex(x = x, pattern = pattern)
    expect_true(ok)
})

test_that("isNotMatchingRegex : FALSE", {
    x <- c("foobar", "foo")
    pattern <- "^f"
    ok <- isNotMatchingRegex(x = x, pattern = pattern)
    expect_s4_class(ok, "goalie")
    expect_false(any(ok))
    ok <- allAreNotMatchingRegex(x = x, pattern = pattern)
    expect_false(ok)
})
