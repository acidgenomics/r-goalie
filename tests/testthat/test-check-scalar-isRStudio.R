context("check : scalar : isRStudio")

test_that("isRStudio", {
    expect_type(isRStudio(), "logical")
})
