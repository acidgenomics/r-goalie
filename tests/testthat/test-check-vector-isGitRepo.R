context("check : vector : isGitRepo")

## This should return true inside package root and testthat subdirectory,
## but may fail for R CMD checks on tarball.
test_that("TRUE", {
    wd <- getwd()
    ok <- isGitRepo(c(wd, wd))
    expected <- c(TRUE, TRUE)
    names(expected) <- c(wd, wd)
    expect_identical(ok, expected)
})

test_that("FALSE", {
    x <- c("~", "/")
    ok <- isGitRepo(x)
    expect_s4_class(ok, "goalie")
    expect_identical(nocause(ok), c("~" = FALSE, "/" = FALSE))
    expected <- rep("not git repo", times = 2L)
    names(expected) <- x
    expect_identical(cause(ok), expected = expected)
})



context("check : scalar : isAGitRepo")

test_that("TRUE", {
    x <- getwd()
    ok <- isAGitRepo(x)
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isAGitRepo("~")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = c("~" = "not git repo")
    )
})



context("check : scalar : allAreGitRepos")

test_that("TRUE", {
    x <- getwd()
    ok <- allAreGitRepos(x)
    expect_true(ok)
})

test_that("FALSE", {
    x <- c(getwd(), "~")
    ok <- allAreGitRepos(x)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_match(cause(ok), "~")
})
