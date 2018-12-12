context("Is URL?")

test_that("isURL", {
    expect_error(isURL(1L))
    expect_false(isURL("XXX"))
})
