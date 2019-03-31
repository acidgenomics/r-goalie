context("check : vector : hasAccess")

test_that("TRUE", {
    ok <- hasAccess(c("~", "."))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))
})

test_that("FALSE : no access", {
    ok <- hasAccess(c("xxx", "yyy"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(xxx = FALSE, yyy = FALSE))
    expect_identical(cause(ok), noquote(c("no access", "no access")))
})

test_that("FALSE : NULL input", {
    ok <- hasAccess(NULL)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("x is not character."))
})
