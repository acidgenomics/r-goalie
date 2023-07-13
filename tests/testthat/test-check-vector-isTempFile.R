test_that("TRUE", {
    x <- tempfile()
    file.create(x)
    expect_true(isATempFile(x))
    unlink(x)
})

test_that("FALSE", {
    x <- tempfile()
    expect_false(isATempFile(x))
})
