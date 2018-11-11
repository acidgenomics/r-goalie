context("Formal arguments")

test_that("assertFormalCompress", {
    expect_error(
        object = assertFormalCompress("XXX"),
        regexp = paste(
            "is_subset :",
            "The element 'XXX' in object is not in",
            "c\\(\"bzip2\", \"gzip\", \"xz\"\\)."
        )
    )
    expect_silent(assertFormalCompress("xz"))
})
