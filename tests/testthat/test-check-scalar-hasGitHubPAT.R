context("check : scalar : hasGitHubPAT")

test_that("TRUE", {
    Sys.setenv("GITHUB_PAT" = "XXX")
    x <- hasGitHubPAT()
    expect_true(x)
})

test_that("FALSE", {
    Sys.setenv("GITHUB_PAT" = "")
    x <- hasGitHubPAT()
    expect_false(x)
})
