context("check : scalar : isDocker")

test_that("isDocker", {
    expect_type(isDocker(), "logical")
})
