skip_if_not_installed("withr")

test_that("TRUE", {
    withr::with_envvar(
        new = c("GITHUB_PAT" = "XXX"),
        code = {
            ok <- hasGitHubPAT()
            expect_true(ok)
        }
    )
})

test_that("FALSE", {
    withr::with_envvar(
        new = c("GITHUB_PAT" = ""),
        code = {
            ok <- hasGitHubPAT()
            expect_false(ok)
        }
    )
})
