context("isDirectory")

test_that("TRUE", {
    ok <- isDirectory(c("~", "."))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))
})

test_that("FALSE : not dir", {
    ok <- isDirectory(c("aaa", "bbb"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(aaa = FALSE, bbb = FALSE))
    expect_identical(cause(ok), noquote(c("not dir", "not dir")))
})

test_that("FALSE : NULL input", {
    ok <- isDirectory(NULL)
    expect_false(ok)
})
