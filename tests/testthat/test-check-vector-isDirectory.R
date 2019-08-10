context("isDirectory")

test_that("TRUE", {
    ok <- isDirectory(c("~", "."))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))
})

test_that("FALSE : not dir", {
    x <- c("aaa", "bbb")
    ok <- isDirectory(x)
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(aaa = FALSE, bbb = FALSE))
    expected <- noquote(c("not dir", "not dir"))
    names(expected) <- x
    expect_identical(cause(ok), expected = expected)
})

test_that("FALSE : NULL input", {
    ok <- isDirectory(NULL)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("'x' is not character."))
})



context("isADirectory")

test_that("TRUE", {
    ok <- isADirectory(".")
    expect_true(ok)
})

test_that("FALSE : not dir", {
    ok <- isADirectory("aaa")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote(c(aaa = "not dir"))
    )
})

test_that("FALSE : not scalar", {
    ok <- isADirectory(c("~", "."))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("'x' is not a character of length 1.")
    )
})

test_that("nullOK", {
    expect_false(isADirectory(NULL, nullOK = FALSE))
    expect_true(isADirectory(NULL, nullOK = TRUE))
})



context("allAreDirectories")

test_that("TRUE", {
    ok <- allAreDirectories(c("~", "."))
    expect_true(ok)
})

test_that("FALSE", {
    ok <- allAreDirectories(c(".", "bbb"))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("1   2   bbb not dir")
    )
})
