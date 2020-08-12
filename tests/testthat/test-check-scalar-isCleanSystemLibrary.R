context("check : scalar : isCleanSystemLibrary")

test_that("isCleanSystemLibrary", {
    expect_type(isCleanSystemLibrary(), "logical")
})
