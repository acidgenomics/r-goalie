test_that("TRUE", {
    ok <- isDirectory(c("~", "."))
    expect_identical(ok, rep(TRUE, 2L))
})

test_that("FALSE : not dir", {
    x <- c("aaa", "bbb")
    ok <- isDirectory(x)
    expect_s4_class(ok, "goalie")
    expect_identical(nocause(ok), rep(FALSE, 2L))
    expect_identical(cause(ok), rep("not dir", 2L))
})

test_that("FALSE : NULL input", {
    ok <- isDirectory(NULL)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var NULL} has length 0."
    )
})



test_that("TRUE", {
    ok <- isADirectory(".")
    expect_true(ok)
})

test_that("FALSE : not dir", {
    ok <- isADirectory("aaa")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), "not dir")
})

test_that("FALSE : not scalar", {
    ok <- isADirectory(c("~", "."))
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var character} doesn't have a length of 1."
    )
})

test_that("nullOk", {
    expect_false(isADirectory(NULL, nullOk = FALSE))
    expect_true(isADirectory(NULL, nullOk = TRUE))
})



test_that("TRUE", {
    ok <- allAreDirectories(c("~", "."))
    expect_true(ok)
})

test_that("FALSE", {
    ok <- allAreDirectories(c(".", "bbb"))
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_match(cause(ok), "2")
})
