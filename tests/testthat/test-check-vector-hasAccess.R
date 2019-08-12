context("check : vector : hasAccess")

test_that("TRUE", {
    x <- c("~", ".")

    ok <- hasAccess(x, access = "rwx")
    expect_true(all(ok))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))

    ok <- allHaveAccess(x, access = "rwx")
    expect_true(ok)
})

test_that("FALSE : no access", {
    x <- c("xxx", "yyy")

    ok <- hasAccess(x)
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(xxx = FALSE, yyy = FALSE))
    expected <- noquote(c("no access", "no access"))
    names(expected) <- x
    expect_identical(cause(ok), expected)

    ok <- allHaveAccess(x)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})

test_that("FALSE : NULL input", {
    ok <- hasAccess(NULL)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote("'x' is not character.")
    )
})

test_that("FALSE : Invalid access codes", {
    ok <- hasAccess(".", access = "xxx")
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        cause(ok),
        noquote(paste0(
            "'x' doesn't contain valid access codes.", "\n",
            "Combinations of 'r', 'w' and 'x' are allowed."
        ))
    )
})
