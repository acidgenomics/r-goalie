urls <- c("https://www.r-project.org", "ftp://r-project.org")

test_that("isURL", {
    isURL(urls)
    isAURL(urls[[1L]])
    allAreURLs(urls)
    ok <- isURL("xxx")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = c("xxx" = "not URL")
    )
})

test_that("URL connection support", {
    x <- url(urls[[1L]])
    expect_s3_class(x, "url")
    expect_true(isAURL(x))
    close(x)
})

test_that("isAURL", {
    expect_true(isAURL(urls[[1L]]))
    ok <- isAURL(urls)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var urls} doesn't have a length of 1."
    )
})

test_that("allAreURLs", {
    expect_true(allAreURLs(urls))
})

test_that("Encoding", {
    url <- "https://rest.ensembl.org/info/assembly/Homo%20sapiens"
    expect_true(isAURL(url))
    url <- "https://rest.ensembl.org/info/assembly/Homo sapiens"
    expect_false(isAURL(url))
})
