skip_if_not_installed("withr")

test_that("TRUE", {
    withr::with_envvar(
        new = c(
            "CONDA_DEFAULT_ENV" = "test",
            "CONDA_SHLVL" = "2"
        ),
        code = {
            ok <- isCondaEnabled()
            expect_true(ok)
        }
    )
    withr::with_envvar(
        new = c(
            "CONDA_DEFAULT_ENV" = "base",
            "CONDA_SHLVL" = "1"
        ),
        code = {
            ok <- isCondaEnabled(ignoreBase = FALSE)
            expect_true(ok)
        }
    )
})

test_that("FALSE", {
    withr::with_envvar(
        new = c(
            "CONDA_DEFAULT_ENV" = "",
            "CONDA_SHLVL" = ""
        ),
        code = {
            ok <- isCondaEnabled()
            expect_s4_class(ok, "goalie")
            expect_false(ok)
            expect_identical(cause(ok), "Conda is not enabled.")
        }
    )
    withr::with_envvar(
        new = c(
            "CONDA_DEFAULT_ENV" = "base",
            "CONDA_SHLVL" = "1"
        ),
        code = {
            ok <- isCondaEnabled(ignoreBase = TRUE)
            expect_s4_class(ok, "goalie")
            expect_false(ok)
            expect_identical(
                object = cause(ok),
                expected = "Ignoring active conda {.val base} environment."
            )
        }
    )
})
