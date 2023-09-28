urls <- c(
    "https://www.r-project.org/",
    "ftp://r-project.org/",
    "https://ndownloader.figshare.com/files/35020903",
    "https://figshare.com/ndownloader/files/40448834"
)

test_that("isExistingUrl", {
    ok <- isExistingUrl(urls)
    expect_true(all(ok))
    ok <- isExistingUrl("https://failwhale.acidgenomics.com/")
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = c(
            "https://failwhale.acidgenomics.com/" = "URL doesn't exist"
        )
    )
})

test_that("URL connection support", {
    x <- url(urls[[1L]])
    expect_s3_class(x, "url")
    ok <- isAnExistingUrl(x)
    expect_true(ok)
    close(x)
})

test_that("Only HTTP(S) and FTP", {
    ok <- isAnExistingUrl("gopher://foobar")
    expect_false(ok)
    expect_match(cause(ok), "doesn't match")
    ok <- isAnExistingUrl("sftp://sftp-private.ncbi.nlm.nih.gov/")
    expect_false(ok)
    expect_match(cause(ok), "doesn't match")
})

test_that("isAnExistingUrl", {
    ok <- isAnExistingUrl(urls[[1L]])
    expect_true(ok)
    ok <- isAnExistingUrl(urls)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var x} doesn't have a length of 1."
    )
})

test_that("allAreExistingUrls", {
    ok <- allAreExistingUrls(urls)
    expect_true(ok)
})