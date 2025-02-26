test_that("TRUE", {
    x <- tempfile()
    invisible(file.create(x))
    expect_true(isATempFile(x))
    expect_true(allAreTempFiles(x))
    unlink(x)
})

test_that("FALSE", {
    x <- tempfile()
    expect_false(nocause(isATempFile(x)))
    expect_false(nocause(allAreTempFiles(x)))
})
