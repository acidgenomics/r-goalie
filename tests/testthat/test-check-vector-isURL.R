urls <- c("https://www.r-project.org", "ftp://r-project.org")

test_that("isURL", {
    ok <- isURL(urls)
    expect_true(all(ok))
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
    ok <- isAURL(x)
    expect_true(ok)
    close(x)
})

test_that("isAURL", {
    ok <- isAURL(urls[[1L]])
    expect_true(ok)
    ok <- isAURL(urls)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var urls} doesn't have a length of 1."
    )
})

test_that("allAreURLs", {
    ok <- allAreURLs(urls)
    expect_true(ok)
})

test_that("Encoding", {
    url <- "https://rest.ensembl.org/info/assembly/Homo%20sapiens"
    ok <- isAURL(url)
    expect_true(ok)
    url <- "https://rest.ensembl.org/info/assembly/Homo sapiens"
    ok <- isAURL(url)
    expect_false(ok)
})
