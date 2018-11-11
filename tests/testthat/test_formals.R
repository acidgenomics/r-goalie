context("Formal arguments")

test_that("assertFormalCompress", {
    expect_error(
        object = assertFormalCompress("XXX"),
        regexp = "gzip"
    )
    expect_silent(assertFormalCompress("xz"))
})
