skip_if_not_installed("withr")

test_that("TRUE", {
    withr::with_options(
        new = list("acid.dark" = TRUE),
        code = {
            ok <- isDark()
            expect_true(ok)
        }
    )
})

test_that("FALSE", {
    withr::with_options(
        new = list("acid.dark" = NULL),
        code = {
            ok <- isDark()
            expect_false(ok)
            expect_s4_class(ok, "goalie")
            expect_identical(
                object = cause(ok),
                expected = "Dark mode is not enabled."
            )
        }
    )
})
