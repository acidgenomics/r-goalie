context("isFile")

test_that("TRUE", {
    x <- "example.txt"
    file.create(x)
    expect_true(isAFile(x))
    unlink(x)
})

# Directories currently return TRUE, similar to base R `dir.exists()`.
# May want to tighten this in a future update to actual files only.
test_that("TRUE : directory input", {
    ok <- isFile(c("~", "."))
    expect_identical(ok, c(`~` = TRUE, `.` = TRUE))
})

test_that("FALSE : not file", {
    ok <- isFile(c("aaa", "bbb"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(aaa = FALSE, bbb = FALSE))
    expect_identical(cause(ok), noquote(c("not file", "not file")))
})

test_that("FALSE : not character", {
    ok <- isFile(1L)
    expect_s3_class(ok, "goalie")
    expect_false(ok)
    expect_identical(cause(ok), noquote("x is not character."))
})
