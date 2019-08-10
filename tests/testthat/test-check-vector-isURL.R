context("isURL")

urls <- c("https://www.r-project.org", "ftp://r-project.org")

test_that("isURL", {
    isURL(urls)
    isAURL(urls[[1L]])
    allAreURLs(urls)

    ok <- isURL("xxx")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote(c(xxx = "not URL"))
    )

    ok <- isAURL(urls)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("'urls' is not a character of length 1.")
    )
})

test_that("isAURL", {
    expect_true(isAURL(urls[[1L]]))

    ok <- isAURL(urls)
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("'urls' is not a character of length 1.")
    )
})

test_that("allAreURLs", {
    expect_true(allAreURLs(urls))
})
