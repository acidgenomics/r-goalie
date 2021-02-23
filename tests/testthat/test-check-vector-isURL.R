context("check : vector : isURL")

urls <- c("https://www.r-project.org", "ftp://r-project.org")

test_that("isURL", {
    isURL(urls)
    isAURL(urls[[1L]])
    allAreURLs(urls)

    ok <- isURL("xxx")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        c(xxx = "not URL")
    )
})

test_that("URL connection support", {
    x <- url(urls[[1L]])
    expect_s4_class(x, "url")
    expect_true(isAURL(x))
})

test_that("isAURL", {
    expect_true(isAURL(urls[[1L]]))

    ok <- isAURL(urls)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        "'urls' does not have a length of 1."
    )
})

test_that("allAreURLs", {
    expect_true(allAreURLs(urls))
})
