context("check : scalar : isCondaEnabled")

test_that("TRUE", {
    vars <- c("CONDA_DEFAULT_ENV", "CONDA_SHLVL")
    Sys.unsetenv(vars)
    Sys.setenv(
        "CONDA_DEFAULT_ENV" = "test",
        "CONDA_SHLVL" = "2"
    )
    ok <- isCondaEnabled()
    expect_true(ok)
    Sys.unsetenv(vars)
    Sys.setenv(
        "CONDA_DEFAULT_ENV" = "base",
        "CONDA_SHLVL" = "1"
    )
    ok <- isCondaEnabled(ignoreBase = FALSE)
    expect_true(ok)
    Sys.unsetenv(vars)
})

test_that("FALSE", {
    vars <- c("CONDA_DEFAULT_ENV", "CONDA_SHLVL")
    Sys.unsetenv(vars)
    ok <- isCondaEnabled()
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), "Conda is not enabled.")
    Sys.unsetenv(vars)
    Sys.setenv(
        "CONDA_DEFAULT_ENV" = "base",
        "CONDA_SHLVL" = "1"
    )
    ok <- isCondaEnabled(ignoreBase = TRUE)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        "Ignoring active conda {.val base} environment."
    )
    Sys.unsetenv(vars)
})
