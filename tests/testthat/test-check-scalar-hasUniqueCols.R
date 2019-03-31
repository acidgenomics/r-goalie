context("check : scalar : hasUniqueCols")

test_that("TRUE", {
    x <- matrix(data = seq_len(20L), ncol = 2L)
    expect_true(hasUniqueCols(x))
})

test_that("FALSE", {
    x <- matrix(data = rep(seq_len(10L), times = 2L), ncol = 2L)
    ok <- hasUniqueCols(x)
    expect_false(ok)
    expect_s3_class(ok, "goalie")
    expect_match(
        as.character(cause(ok)),
        "has duplicated columns"
    )
})
