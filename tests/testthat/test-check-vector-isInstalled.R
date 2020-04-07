context("check : vector : isInstalled")

test_that("TRUE", {
    pkgs <- c("base", "utils")
    expect_true(all(isInstalled(pkgs)))
    expect_true(allAreInstalled(pkgs))
})

test_that("FALSE", {
    pkgs <- c("AAA", "BBB")
    expect_false(all(isInstalled(pkgs)))
    expect_false(allAreInstalled(pkgs))
})
