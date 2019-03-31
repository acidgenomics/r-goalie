context("isExisting")

test_that("TRUE", {
    a <- 1L
    b <- 2L

    ok <- isExisting(c("a", "b"))
    expect_identical(ok, c(a = TRUE, b = TRUE))

    ok <- allAreNonExisting(c("x", "y"))
    expect_true(ok)
})

test_that("FALSE", {
    ok <- isExisting(c("x", "y"))
    expect_s3_class(ok, "goalie")
    expect_identical(nocause(ok), c(x = FALSE, y = FALSE))
    expect_identical(cause(ok), noquote(c("non-existing", "non-existing")))

    ok <- allAreExisting(c("x", "y"))
    expect_s3_class(ok, "goalie")
    expect_false(ok)
})
