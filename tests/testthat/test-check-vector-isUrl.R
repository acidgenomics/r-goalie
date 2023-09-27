urls <- c("https://www.r-project.org/", "ftp://r-project.org/")

test_that("isUrl", {
    ok <- isUrl(urls)
    expect_true(all(ok))
    ok <- isUrl("xxx")
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
    ok <- isAUrl(x)
    expect_true(ok)
    close(x)
})

test_that("isAUrl", {
    ok <- isAUrl(urls[[1L]])
    expect_true(ok)
    ok <- isAUrl(urls)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var x} doesn't have a length of 1."
    )
})

test_that("allAreUrls", {
    ok <- allAreUrls(urls)
    expect_true(ok)
})

test_that("Encoding", {
    url <- "https://rest.ensembl.org/info/assembly/Homo%20sapiens"
    ok <- isAUrl(url)
    expect_true(ok)
    url <- "https://rest.ensembl.org/info/assembly/Homo sapiens"
    ok <- isAUrl(url)
    expect_false(ok)
})
