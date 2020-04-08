context("check : vector : isInstalled")

test_that("TRUE", {
    x <- c("base", "utils")
    expect_true(all(isInstalled(x)))
    expect_true(allAreInstalled(x))
})

test_that("FALSE", {
    x <- c("AAA", "BBB")
    expect_false(all(isInstalled(x)))
    expect_false(allAreInstalled(x))
})
