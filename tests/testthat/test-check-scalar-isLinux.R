context("check : scalar : isLinux")

test_that("logical", {
    expect_type(isLinux(), "logical")
})
