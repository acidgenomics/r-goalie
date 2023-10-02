test_that("TRUE", {
    x <- c("~", ".")
    ok <- hasAccess(x, access = "rwx")
    expect_identical(ok, rep(TRUE, 2L))
    ok <- allHaveAccess(x, access = "rwx")
    expect_true(ok)
})

test_that("FALSE : no access", {
    x <- c("xxx", "yyy")
    ok <- hasAccess(x)
    expect_s4_class(ok, "goalie")
    expect_identical(nocause(ok), rep(FALSE, 2L))
    expect_identical(cause(ok), rep("no access", 2L))
    ok <- allHaveAccess(x)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
})

test_that("FALSE : NULL input", {
    ok <- hasAccess(NULL)
    expect_s4_class(ok, "goalie")
    expect_false(ok)
    expect_identical(
        object = cause(ok),
        expected = "{.var NULL} has length 0."
    )
})
