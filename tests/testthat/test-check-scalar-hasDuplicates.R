context("hasDuplicates")

test_that("TRUE", {
    expect_true(hasDuplicates(c("a", "a")))
})

test_that("FALSE", {
    ok <- hasDuplicates(c("a", "b"))
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote('c("a", "b") has no duplicates.')
    )
})



context("hasNoDuplicates")

test_that("TRUE", {
    expect_true(hasNoDuplicates(c("a", "b")))
})

test_that("FALSE", {
    ok <- hasNoDuplicates(c("a", "a", "b", "b"))
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_identical(
        cause(ok),
        noquote('c("a", "a", "b", "b") has duplicates at positions 2, 4.')
    )
})
