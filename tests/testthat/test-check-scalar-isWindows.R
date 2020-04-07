context("check : scalar : isWindows")

test_that("logical", {
    expect_type(isWindows(), "logical")
})
