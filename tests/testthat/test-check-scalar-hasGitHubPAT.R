context("check : scalar : hasGitHubPAT")

test_that("GITHUB_PAT environment variable", {
    Sys.setenv("GITHUB_PAT" = "XXX")
    x <- hasGitHubPAT()
    expect_true(x)
})
