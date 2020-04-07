context("check : scalar : hasGitHubPAT")

test_that("hasGitHubPAT", {
    x <- hasGitHubPAT()
    expect_type(x, "logical")
})
