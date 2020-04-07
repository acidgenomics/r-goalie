context("check : scalar : isMacOS")

test_that("logical", {
    expect_type(isMacOS(), "logical")
})
