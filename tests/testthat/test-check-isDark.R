test_that("TRUE", {
    opt <- getOption("acid.dark")
    options("acid.dark" = TRUE)
    ok <- isDark()
    expect_true(ok)
    options("acid.dark" = opt)
})

test_that("FALSE", {
    opt <- getOption("acid.dark")
    options("acid.dark" = NULL)
    ok <- isDark()
    expect_false(ok)
    expect_s4_class(ok, "goalie")
    expect_identical(
        object = cause(ok),
        expected = "Dark mode is not enabled."
    )
})
